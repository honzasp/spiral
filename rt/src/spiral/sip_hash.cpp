#include "spiral/sip_hash.hpp"

namespace spiral {
  static void sip_process_block(SipHasher* hasher, uint64_t m);
  static void sip_round(SipHasher* hasher);

  static void sip_process_block(SipHasher* hasher, uint64_t m) {
    hasher->v3 ^= m;
    sip_round(hasher);
    sip_round(hasher);
    hasher->v0 ^= m;
  }

  static void sip_round(SipHasher* hasher) {
    auto rotl = [](uint64_t x, uint32_t b) -> uint64_t {
      return (x << b) | (x >> (64 - b));
    };

    hasher->v0 += hasher->v1;
    hasher->v1 = rotl(hasher->v1, 13) ^ hasher->v0;
    hasher->v0 = rotl(hasher->v0, 32);

    hasher->v2 += hasher->v3;
    hasher->v3 = rotl(hasher->v3, 16) ^ hasher->v2;

    hasher->v0 += hasher->v3;
    hasher->v3 = rotl(hasher->v3, 21) ^ hasher->v0;

    hasher->v2 += hasher->v1;
    hasher->v1 = rotl(hasher->v1, 17) ^ hasher->v2;
    hasher->v2 = rotl(hasher->v2, 32);
  }

  auto sip_new(uint64_t k0, uint64_t k1) -> SipHasher {
    SipHasher hasher;
    hasher.k0 = k0;
    hasher.k1 = k1;
    hasher.v0 = 0x736f6d6570736575ull ^ k0;
    hasher.v1 = 0x646f72616e646f6dull ^ k1;
    hasher.v2 = 0x6c7967656e657261ull ^ k0;
    hasher.v3 = 0x7465646279746573ull ^ k1;
    hasher.total_len = 0;
    hasher.tail = 0;
    hasher.tail_len = 0;
    return hasher;
  }

  uint64_t sip_finish(SipHasher* hasher) {
    uint64_t b = ((uint64_t(hasher->total_len & 0xff)) << 56) | hasher->tail;
    sip_process_block(hasher, b);

    hasher->v2 ^= 0xff;

    sip_round(hasher);
    sip_round(hasher);
    sip_round(hasher);
    sip_round(hasher);
    return hasher->v0 ^ hasher->v1 ^ hasher->v2 ^ hasher->v3;
  }

  void sip_push_bytes(SipHasher* hasher, const uint8_t* bytes, uint32_t len) {
    uint32_t i = 0;
    for(; hasher->tail_len != 0 && i < len; ++i) {
      hasher->tail |= (uint64_t(bytes[i]) << (8 * hasher->tail_len));
      if(++hasher->tail_len >= 8) {
        sip_process_block(hasher, hasher->tail);
        hasher->tail_len = 0;
        hasher->tail = 0;
      }
    }

    assert(i + 8 >= len || hasher->tail_len == 0);
    for(; i + 8 <= len; i += 8) {
      uint64_t block =
        (uint64_t(bytes[i + 0]) << 0) |
        (uint64_t(bytes[i + 1]) << 8) |
        (uint64_t(bytes[i + 2]) << 16) |
        (uint64_t(bytes[i + 3]) << 24) |
        (uint64_t(bytes[i + 4]) << 32) |
        (uint64_t(bytes[i + 5]) << 40) |
        (uint64_t(bytes[i + 6]) << 48) |
        (uint64_t(bytes[i + 7]) << 56);
      sip_process_block(hasher, block);
    }

    for(; i < len; ++i) {
      hasher->tail |= (uint64_t(bytes[i]) << (8 * hasher->tail_len));
      ++hasher->tail_len;
      assert(hasher->tail_len < 8);
    }

    hasher->total_len += len;
  }

  void sip_push_u8(SipHasher* hasher, uint8_t byte) {
    hasher->tail |= (uint64_t(byte) << (8 * hasher->tail_len));
    if(++hasher->tail_len >= 8) {
      sip_process_block(hasher, hasher->tail);
      hasher->tail_len = 0;
      hasher->tail = 0;
    }
    ++hasher->total_len;
  }

  void sip_push_u32(SipHasher* hasher, uint32_t word) {
    if(hasher->tail_len <= 4) {
      hasher->tail |= (uint64_t(word) << (8 * hasher->tail_len));
      hasher->tail_len += 4;
      if(hasher->tail_len == 8) {
        sip_process_block(hasher, hasher->tail);
        hasher->tail_len = 0;
        hasher->tail = 0;
      }
      hasher->total_len += 4;
    } else {
      sip_push_u8(hasher, word & 0xff);
      sip_push_u8(hasher, (word >> 8) & 0xff);
      sip_push_u8(hasher, (word >> 16) & 0xff);
      sip_push_u8(hasher, (word >> 24) & 0xff);
    }
  }

  void sip_push_u64(SipHasher* hasher, uint64_t word) {
    if(hasher->tail_len == 0) {
      sip_process_block(hasher, word);
      hasher->total_len += 8;
    } else {
      sip_push_u32(hasher, uint32_t(word & 0xffffffff));
      sip_push_u32(hasher, uint32_t((word >> 32) & 0xffffffff));
    }
  }

  void sip_align(SipHasher* hasher) {
    if(hasher->tail_len != 0) {
      sip_process_block(hasher, hasher->tail);
      hasher->total_len += (8 - hasher->tail_len);
      hasher->tail_len = 0;
      hasher->tail = 0;
    }
  }
}
