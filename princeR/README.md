# PrInCE Scripts

Scripts to run batch jobs of PrInCE in R and Matlab with an array of different input files and classifiers. Uses the Corum coreComplexes complex set as the gold standard.

## Scripts
* "ppis"
    - Runs profiles on R version of PrInCE to generate interactions and associated runtime file
* "bench" 
    - Runs profiles on R version of PrInCE and appends runtime and RAM useage to an rbenchmarks.rds file
* "mlppis"
    - Runs profiles on the MatLab version of PrInCE and generates a .benchmark file with runtime and RAM usage