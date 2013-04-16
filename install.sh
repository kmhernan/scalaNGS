#!/bin/bash

echo "Installing..."
scalac -cp src/com/:src/com/lib/:src/com/read/ -d bin/ \
       src/main/scala/RADtools.scala \
       src/com/read/read.scala \
       src/com/lib/OptionParser.scala

echo "Finished..."
exit 0
