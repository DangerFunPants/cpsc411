#!/bin/bash

for file in $1/*.m+
do
printf "File: $file\n" 1>&2
./mcc < "$file"
printf "\n\n"
done