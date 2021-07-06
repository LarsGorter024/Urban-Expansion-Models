#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=4-00:00:00
#SBATCH --job-name=DEM
#SBATCH --nodes=1
#SBATCH --mem=48G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/DEM.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/Data_processing/DEM_variable.R
