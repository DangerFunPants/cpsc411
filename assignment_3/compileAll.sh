#!/bin/bash

for file in $1/*.m+
do
printf "File: $file\n" 1>&2
outFile=`basename ${file}`
./mcc < "$file"
printf "\n\n"
done