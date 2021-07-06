#!/bin/bash

#SBATCH --partition=milkun
#SBATCH --time=1:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --job-name=mosaic_tiles
#SBATCH --mem-per-cpu=64G
#SBATCH --array=1
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Mosaic/mosaic_tile_%a.out"
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Mosaic/mosaic_tile_%a.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

export TMPDIR=/scratch/mdls
mkdir -p $TMPDIR

srun /opt/R-4.0.4/bin/R --vanilla --no-save --args ${SLURM_ARRAY_TASK_ID} < /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global//mosaic_pred_tiles.R
