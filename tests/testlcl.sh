#!/bin/bash

export W=../wajsberg

for i in ILTP-v1.1.2-propositional/Problems/LCL/*.p; do 
  res=`$W $i`
  echo -n "Test $i, result: "
  exp=`grep Status $i`
  if echo $exp | grep "Non-Theorem" > /dev/null; then
    if echo $res | grep OK > /dev/null; then
      echo "[ Fail ]"
    else
      echo "[ OK ]"
    fi
  elif  echo $exp | grep "Theorem" > /dev/null; then
    if echo $res | grep OK > /dev/null; then
      echo "[ OK ]"
    else
      echo "[ Fail ]"
    fi
  fi
done
