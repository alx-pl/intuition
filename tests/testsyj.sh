#!/bin/bash

export W=../wajsberg

for i in ILTP-v1.1.2-propositional/Problems/SYJ/*.p; do 
  res=`timeout 1m $W $i`
  echo -n "Test $i, result: "
  exp=`grep Status $i`
  if echo $exp | grep "Non-Theorem" > /dev/null; then
    if echo $res | grep OK > /dev/null; then
      echo "[ Fail ]"
    elif echo $res | grep Fail > /dev/null; then
      echo "[ OK ]"
    else
      echo "[ Timeout ]"
    fi
  elif  echo $exp | grep "Theorem" > /dev/null; then
    if echo $res | grep OK > /dev/null; then
      echo "[ OK ]"
    elif echo $res | grep Fail > /dev/null; then
      echo "[ Fail ]"
    else 
      echo "[ Timeout ]"
    fi
  elif echo $exp | grep "Unsolved" > /dev/null; then
    if echo $res | grep OK > /dev/null; then
      echo "[ OK ], but unsolved"
    elif echo $res | grep Fail > /dev/null; then
      echo "[ Fail ], but unsolved"
    else
      echo "[ Timeout ], but unsolved"
    fi
  fi
done
