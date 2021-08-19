#!/bin/bash
for i in ./*crn; do timeout 600 ../../../../../ModellingEngine/CliBME/bin/Release/CliBME.exe -simulator Cme -simulate  "$i" && echo $MSG; done
for i in ./*crn; do timeout 600 ../../../../../ModellingEngine/CliBME/bin/Release/CliBME.exe -simulator CmeStiff -simulate  "$i" && echo $MSG; done
