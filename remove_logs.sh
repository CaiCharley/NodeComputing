#!/bin/bash
# Removes log file of jobs that completed successfully
for file in "$1"/*
do
    if [[ $(sed -n '$p' $file) == "Done" ]]; then
    rm $file
    fi
done