#!/bin/sh
#SBATCH --job-name=hw2
#SBATCH --mail-type=ALL
#SBATCH --mail-user=josesa@ucr.edu
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=2gb
#SBATCH --time=4:00:00
#SBATCH --output=300_SimExploringHwglSparsity/experiments1/logs/output%a.out
#SBATCH --array=970-1929

pwd; hostname; date

echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo This is task $SLURM_ARRAY_TASK_ID,
echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo a
echo a

echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo 1 Importing R and Rscript:
module load r/4.1.3
echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo a
echo a

echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo 3 Running 300_SimExploringHwglSparsity/123_SimulationScript.R with input $SLURM_ARRAY_TASK_ID 2
Rscript 300_SimExploringHwglSparsity/123_SimulationScript.R $SLURM_ARRAY_TASK_ID 2
echo aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
echo a
echo a

date
