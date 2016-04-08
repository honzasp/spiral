#ifndef HAVE_spiral_sip_hash_hpp
#define HAVE_spiral_sip_hash_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct SipHasher {
    uint64_t k0, k1;
    uint64_t v0, v2;
    uint64_t v1, v3;
    uint32_t total_len;
    uint64_t tail;
    uint32_t tail_len;
  };

  auto sip_new(uint64_t k0, uint64_t k1) -> SipHasher;
  void sip_push_bytes(SipHasher* hasher, const uint8_t* bytes, uint32_t len);
  void sip_push_u8(SipHasher* hasher, uint8_t byte);
  void sip_push_u32(SipHasher* hasher, uint32_t word);
  void sip_push_u64(SipHasher* hasher, uint64_t word);
  void sip_align(SipHasher* hasher);
  uint64_t sip_finish(SipHasher* hasher);
}
#endif
