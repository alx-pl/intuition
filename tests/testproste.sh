#!/bin/bash

export W=../prover-dpll

for i in proste/*.p; do 
  res=`$W -f $i`
  echo -n "Test $i, result: "
  if echo $i | grep "dobry" > /dev/null; then
    if echo $res | grep '\[Just (MInt' > /dev/null; then
      echo "[ OK ]"
    else
      echo "[ Fail ]"
    fi
  else
    if echo $res | grep OK > /dev/null; then
      echo "[ Fail ]"
    else
      echo "[ OK ]"
    fi
  fi
done
