#!/bin/bash
cd $(dirname $0)
SPIRAL="../target/debug/spiral"
RUNTIME="../rt/build/runtime.a"
GREEN="\e[32m"
RED="\e[31m"
CLEAR="\e[0m"

ok_count=0
err_count=0
for test_file in *.spiral
do
  printf "test %s\n" "$test_file"
  exec_file=`echo "$test_file" | sed 's/spiral/exec/'`
  output_file=`echo "$test_file" | sed 's/spiral/out/'`
  real_output_file="${output_file}~"

  rm -f "$exec_file" "$real_output_file"

  ok=""
  if "$SPIRAL" "$test_file" --output "$exec_file" --runtime "$RUNTIME"
  then
    ./"$exec_file" >"$real_output_file" 2>&1
    if diff "$output_file" "$real_output_file" >/dev/null 
    then
      ok="yes"
    fi
  fi

  if [ -n "$ok" ] 
  then
    printf "  ${GREEN}passed${CLEAR}\n"
    rm -f "$real_output_file" "$exec_file"
    ok_count=$((ok_count+1))
  else
    printf "  ${RED}FAILED${CLEAR}\n"
    err_count=$((err_count+1))
  fi
done

if [ $err_count = 0 ]
then
  printf "${GREEN}%d tests passed${CLEAR}\n" $ok_count
else
  printf "${RED}%d tests from %d failed${CLEAR}\n" $err_count \
    $((err_count+ok_count))
fi
