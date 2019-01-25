#!/bin/bash
pw="*D5jkhgrlj"
host="alexander.james@linux.cpsc.ucalgary.ca"
compilerBin=mcc

amArgs='/usr/bin/sml @SMLload=/home/411/M/M.x86-linux ~/src.m+;exit'
rmCommand='rm ~/src.am;exit'
sshpass -p "$pw" scp $1 "$host":~/src.m+
sshpass -p "$pw" ssh "$host" "$amArgs"
sshpass -p "$pw" scp "$host":~/src.am ./
sshpass -p "$pw" ssh "$host" "$rmCommand"