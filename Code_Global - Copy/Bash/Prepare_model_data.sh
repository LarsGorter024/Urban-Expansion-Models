#!/bin/bash


#SBATCH --partition=milkun
#SBATCH --time=24:00:00
#SBATCH --job-name=Prepare_model_data
#SBATCH --nodes=1
#SBATCH --mem=64G
#SBATCH --output "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Logs/Prepare_model_data.log"
#SBATCH --mail-type=END
#SBATCH --mail-user=l.gorter@student.ru.nl

module load R-4.0.4
Rscript /vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Code_Global/prepare_model_data.R
