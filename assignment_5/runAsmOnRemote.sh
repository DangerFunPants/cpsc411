#!/bin/bash
pw="*D5jkhgrlj"
host="alexander.james@linux.cpsc.ucalgary.ca"
compilerBin="mcc" 

if [ "$2" = "debug" ]; then
    amArgs='/usr/bin/sml @SMLload=/home/411/AM/am+.x86-linux -d ~/of.am+;exit'
else
    amArgs='/usr/bin/sml @SMLload=/home/411/AM/am+.x86-linux ~/of.am+;exit'
fi
# amArgs='/usr/bin/sml @SMLload=/home/411/AM/am+.x86-linux ~/of.am+;exit'
sshpass -p "$pw" scp ./$1 "$host":~/of.am+
sshpass -p "$pw" ssh "$host" "$amArgs"