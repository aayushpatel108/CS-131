#!/bin/sh
for state in AcmeSafe Null Unsynchronized Synchronized
do
  echo "---------------------------"
  echo "VARYING THREADCOUNTS FOR $state"
  echo "---------------------------"
  echo
  for threadcount in 1 8 16 24 32 40
  do
    echo
    echo "$state, $threadcount"
    time timeout 3600 java UnsafeMemory $state $threadcount 100000000 5
    echo
    echo
  done
  echo 

  echo "---------------------------"
  echo "VARYING SIZE FOR $state"
  echo "---------------------------"
  echo
  for size in 5 25 50 75 100 500 
  do
    echo
    echo "$state, $size"
    time timeout 3600 java UnsafeMemory $state 8 100000000 $size
    echo
    echo
  done
  echo 
done



