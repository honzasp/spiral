#!/bin/bash
cd $(dirname $0)
SPIRAL="../target/debug/spiral"
SPIRAL_FLAGS="--include mods --include ../stdlib --opt-level 3"
FAST_RUNTIME="../rt/build/runtime_fast.a"
DEBUG_RUNTIME="../rt/build/runtime_debug.a"
GREEN="\e[32m"
RED="\e[31m"
CLEAR="\e[0m"

ok_count=0
err_count=0
for test_file in `find -name '*-test.spiral'`
do
  if [ -z "$1" ] || echo "$test_file" | egrep "$1" -q
  then
    printf "test %s\n" "$test_file"
    fast_exec_file=`echo "$test_file" | sed 's/spiral/fexec/'`
    debug_exec_file=`echo "$test_file" | sed 's/spiral/dexec/'`
    output_file=`echo "$test_file" | sed 's/spiral/out/'`
    real_output_file="${output_file}~"
    asm_file=`echo "$test_file" | sed 's/spiral/s/'`
    grit_file=`echo "$test_file" | sed 's/spiral/grit/'`
    spine_file=`echo "$test_file" | sed 's/spiral/spine/'`

    rm -f "$fast_exec_file" "$debug_exec_file" "$real_output_file" \
      "$asm_file" "$grit_file" "$spine_file"

    ok=""
    if "$SPIRAL" $SPIRAL_FLAGS "$test_file" \
      --output "$fast_exec_file" --runtime "$FAST_RUNTIME"
    then
      export SPIRAL_TEST_ENV="haskell curry"
      "$fast_exec_file" "alan turing" "alonzo church" >"$real_output_file" 2>&1
      if diff "$output_file" "$real_output_file" >/dev/null 
      then
        ok="yes"
      fi
    fi

    if [ -n "$ok" ] 
    then
      printf "  ${GREEN}passed${CLEAR}\n"
      rm -f "$fast_exec_file" "$debug_exec_file" "$real_output_file" \
        "$asm_file" "$grit_file" "$spine_file"
      ok_count=$((ok_count+1))
    else
      printf "  ${RED}FAILED${CLEAR}\n"
      "$SPIRAL" $SPIRAL_FLAGS "$test_file" \
        --output "$debug_exec_file" --runtime "$DEBUG_RUNTIME"
      if [ "--dump" = "$2" ]
      then
        "$SPIRAL" $SPIRAL_FLAGS "$test_file" --output "$asm_file" --emit gas
        "$SPIRAL" $SPIRAL_FLAGS "$test_file" --output "$grit_file" --emit grit
        "$SPIRAL" $SPIRAL_FLAGS "$test_file" --output "$spine_file" --emit spine
      fi
      err_count=$((err_count+1))
    fi
  fi
done

if [ $err_count = 0 ]
then
  printf "${GREEN}%d tests passed.${CLEAR} all bugs are hidden\n" $ok_count
else
  printf "${RED}%d tests from %d failed${CLEAR}\n" $err_count \
    $((err_count+ok_count))
fi
