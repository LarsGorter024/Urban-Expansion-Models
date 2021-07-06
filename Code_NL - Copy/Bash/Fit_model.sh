#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=48:00:00
#SBATCH --job-name=Fit_model
#SBATCH -n 16
#SBATCH --mem=64G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Fit_model.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_NL/fit_model.R
