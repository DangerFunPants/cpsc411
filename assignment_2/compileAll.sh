#!/bin/bash

for file in $1/*.mn
do
printf "File: $file\n"
outFile=`basename ${file}`
./mcc "./stack_asm/$outFile.a" < "$file"
printf "\n\n"
done