#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=4-00:00:00
#SBATCH --job-name=Get_urban_changes
#SBATCH --mem=64G
#SBATCH -N 1 -n 20
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Get_urban_changes.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/Data_processing/Calculate_changes/Get_urban_changes.R
