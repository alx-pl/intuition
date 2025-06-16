#!/bin/bash

export W=../prover-vanilla


function gdzie () {
for i in `ls -1 tst/$1*.p`; do 
  echo "Attempting $i"
  res=`$W $i` > /dev/null
  echo -n "Test $i, result: "
  if echo $res | grep "is Fail" > /dev/null; then 
    echo Fail
  else
    echo OK
  fi
done

}

gdzie test-trzycztery-0

for j in `seq 1 9`; do
  for i in `seq 0 9`; do
    gdzie test-trzycztery-$j$i
  done
done
