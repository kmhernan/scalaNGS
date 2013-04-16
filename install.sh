#!/bin/bash

echo "Installing..."
scalac -cp src/com/:src/com/lib/:src/com/read/:/src/com/tools/ -d bin/ \
       src/main/scala/RADtools.scala \
       src/com/read/read.scala \
       src/com/lib/OptionParser.scala \
       src/com/tools/FilterReads.scala

echo "Finished..."
exit 0
