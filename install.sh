#!/bin/bash

scalac -sourcepath src \
-d bin \
src/scala/com/scalangs/test/ngstest.scala \
src/scala/com/scalangs/io/ioUtil.scala \
src/scala/com/scalangs/fastq/FastqReader.scala

cd bin 
jar -cfm ../Test.jar ../MANIFEST.MF *
cd ../

java -jar Test.jar
