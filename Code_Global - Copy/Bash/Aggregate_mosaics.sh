#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=6:00:00
#SBATCH --job-name=Aggregate_mosaics
#SBATCH --nodes=1
#SBATCH --mem=32G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Aggregate_mosaics.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/aggregate_mosaics.R
