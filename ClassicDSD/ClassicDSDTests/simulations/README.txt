This directory is to contain (potentially) long-running regression tests for the simulators.

A test X comes in the form of two files: X.dna and X.cvs.
X.dna should be a valid DSD program containing a deterministic simulation and X.csv should contain the expected result.
A seldomly (nightly?) run test engine will find all .dna files in this directory and each will be run and the result commpared to the corresponding .csv file.
If no such .csv file exists, the test will not be run.
If at any point the produced values deviate from the expected values beyond the tolerance, the test has failed.
To add a new test to the suite, simply add a .dan file and a cooresponding .csv file to this directory.

For the moment there is only support for deterministic simulations with no sweeps.