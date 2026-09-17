#!/bin/zsh
#SBATCH -A GCIMS
#SBATCH -t 15:00:00
#SBATCH -N 1
##SBATCH -p shared
#SBATCH -p slurm
#SBATCH --array=1-7  # Adjust this based on the number of scenarios
##SBATCH --ntasks-per-node=5
##SBATCH --ntasks=1
##SBATCH --cpus-per-task=12


# Define the CSV file containing the scenarios
scenario_file="scenarios.csv"

# Select the scenario based on the array task ID (assuming the file has one scenario per line)
scenario=$(awk "NR==$SLURM_ARRAY_TASK_ID" $scenario_file)

job=$(awk "NR==$SLURM_ARRAY_TASK_ID" $scenario_file | tr -d '\r\n')

echo 'Library config:'
ldd ./gcam.exe
 
date
time ./gcam.exe -C configuration_$job.xml -Llog_conf.xml
date
