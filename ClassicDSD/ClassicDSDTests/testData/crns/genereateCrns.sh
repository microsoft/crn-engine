#!/bin/bash
SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
SUFFIX=".vdsd"
for f in ../visualDsd/*.vdsd
do
  crn=${f%$SUFFIX}
  # TODO: make the script generate the .crn files in the current folder instead of ../visualDsd
  ../../../../DSD/DNAFS/bin/Debug/dnafs.exe -crn $crn $f
  done
  IFS=$SAVEIFS
