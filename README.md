# Node Computing
Scripts to submit array/batch jobs to compute clusters like Compute Canada

## File Structure
Project folders are stored in the root directory with different array jobs in subdirectories.

## Outer R Script
The outer R script generates a "grid" text file of all the permuations of input files and arguments you want to run with which are user configurable. Additionally, it removes tasks that are complete based on the job's output file from the job from grid file so that the failed jobs can be rerun. The script takes three arguments:
* --project: The name of the project folder
* --name: The name of the job array
* -s: Flag whether to submit the job. Otherwise only updates grid file
* -r: Flag whether to remove output logs of successfully completed jobs

## Outer Helper File
A helper file sourced by the outer R script. Contains variables that are specific to the job as well as job specific functions that the outer script can call. For example to futher mutate the grid file after the call to grid.expand(). Saved with name:
*outer_helper.R

## .sh Batch File
This is the shell file that is submitted to the job scheduler of the given compute cluster (slurm for compute canada). The job specifications like RAM, CPU, and runtime can be specified here. Saved with name:
* jobname_projectname.sh

## Inner Script
Each line of the grid file gets passed to a unique instance of the inner script and can run the specific computation required with the given arguments. Saved with name:
* jobname_projectname.R