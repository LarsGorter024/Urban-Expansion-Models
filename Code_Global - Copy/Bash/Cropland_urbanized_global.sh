#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=6-00:00:00
#SBATCH --job-name=Cropland_urbanized
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=50G
#SBATCH --array=1-251
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Displacement/Cropland_urbanized_%a.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
mkdir /scratch/scratch_lgorter
export TMPDIR=/scratch/scratch_lgorter
srun /opt/R-4.0.4/bin/R --vanilla --no-save --args ${SLURM_ARRAY_TASK_ID} < /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/Extra_scripts/Cropland_displacement.R
rm -rf /scratch/scratch_lgorter