#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=24-00:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=split_predictors
#SBATCH --mem-per-cpu=22G
#SBATCH --array=1-36
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Split_predictors.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

export TMPDIR=/scratch/mdls
mkdir -p $TMPDIR

srun /opt/R-4.0.4/bin/R --vanilla --no-save --args ${SLURM_ARRAY_TASK_ID} < /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_NL/Data_processing/split_predictors.R