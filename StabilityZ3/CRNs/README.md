# Chemical reaction networks (CRNs)

This folder contains analysis methods for CRNs, specifically for assessing Turing instabilities.

## Notes

### Turing-Z3 analysis of (3,4) CRNs produced by genCRN
The readByteEncoding() function in CRNs\ByteEncoding.fs can read the byte-encoded CRNs produced by genCRN.
We have enumerated (3,4) CRNs with equivalence classes {A}{B}{C} and different filters, and then analysed them using StabilityZ3Cli.
Here is a summary of the results:

#### Case 1. 
For non-trivial, connected CRNs:
- SAT - 159
- UNSAT - 148806
- UNKNOWN - 12

Completed: 16189.8 seconds

#### Case 2. 
For non-trivial, connected CRNs without conservation laws:
- SAT - 159
- UNSAT - 88785
- UNKNOWN - 12

Completed: 10005.4 seconds

#### Case 3. 
For non-trivial, connected CRNs without conservation laws and without reactions of the form -> 2A, -> A+B and 2A -> A:
- SAT - 199
UNSAT - 56384
UNKNOWN - 12

Completed: 6791.4 seconds

### Outstanding tasks
The SAT CRN counts do not match, we should investigate why (maybe a bug in the filtering?).

There is also a discrepancy between the SAT CRNs found through genCRN and the ones produced by Enumerator.fs.
We should compare the two sets and find out which CRNs are missing in which set. 
We cannot compare the sets directly using the identity function, because species in the same CRN named differently in the two sets.
To overcome this problem we can produce all (3!) * 159 CRN isomorphs of the genCRN set and then compare the sets using the identity.



### Prompt commands
* Generate non-isomorphic, non-trivial, connected and byte-encoded CRNs:
  
  ```./genCRN.exe -n3 -t -c -b crn_3_4.txt > crn_3_4_tc.byte```
  
  (should produce 25101 CRNs)

* Generate non-isomorphic, non-trivial, connected and byte-encoded CRNs with {A}{B}{C} eq. classes:
    
  ```./genCRN.exe -n3 -t -c -s'1;1;1' -b crn_3_4.txt > crn_3_4_tcs.byte```
  
  (should produce 148977 CRNs)

* Generate non-isomorphic, non-trivial, connected and byte-encoded CRNs with {A}{B}{C} eq. classes and without conservation laws:
  
  ```./genCRN.exe -n3 -t -x -c -s'1;1;1' -b crn_3_4.txt > crn_3_4_tcsx.byte```
  
  (should produce 88956)

* Analyse byte-encoded CRNs:  
  
  ```./StabilityZ3Cli.exe crn --byteencoded --mode batch --file crn_3_4_tcsx.byte --species 3```