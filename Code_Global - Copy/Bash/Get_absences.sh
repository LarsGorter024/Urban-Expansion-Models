#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=4-00:00:00
#SBATCH --job-name=Get_absences
#SBATCH --nodes=1
#SBATCH --mem=254G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Get_absences.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/Data_processing/Absences/Get_absences_fit.R
