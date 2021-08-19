#!/bin/bash
# each line runs a different simulator (Oslo/stiff, Sundials/stiff, LNA/stiff) on all .crn files in the current folder. The simulator is killed if it is taking more than 10 minutes to return the results.
T=600
MSG="Test data stored"
BME="../../../../../../ModellingEngine/CliBME/bin/Release/CliBME.exe -simulator "
for i in ./*crn; do timeout $T $BME deterministic -simulate "$i" && echo $MSG; done
for i in ./*crn; do timeout $T $BME DeterministicStiff -simulate "$i" && echo $MSG; done
for i in ./*crn; do timeout $T $BME Sundials -simulate "$i" && echo $MSG; done
for i in ./*crn; do timeout $T $BME SundialsStiff -simulate "$i" && echo $MSG; done
for i in ./*crn; do timeout $T $BME Lna -simulate "$i" && echo $MSG; done
for i in ./*crn; do timeout $T $BME LnaStiff -simulate "$i" && echo $MSG; done
