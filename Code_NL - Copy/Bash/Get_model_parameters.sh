#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=24:00:00
#SBATCH --job-name=Get_hyperparameters
#SBATCH -n 16
#SBATCH --mem=290G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Get_hyperparameters.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_NL/get_model_parameters.R
