#!/bin/bash

#SBATCH --partition=milkun
#SBATCH --time=1:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=tiled_pred
#SBATCH --mem-per-cpu=32G
#SBATCH --array=1-6630%100
# Standard out and Standard Error output files with the job number in the name.
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Pred_tiles/pred_tile_fit_%a.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

export TMPDIR=/scratch/mdls
mkdir -p $TMPDIR

srun /opt/R-4.0.4/bin/R --vanilla --no-save --args ${SLURM_ARRAY_TASK_ID} < /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/prediction_tiled.R
