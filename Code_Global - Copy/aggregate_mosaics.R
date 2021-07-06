#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=6:00:00
#SBATCH --output "Logs/aggregate_mosaics.out"
#SBATCH --mem=32G


##########################################
####  Aggregate mosaics
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, janitor, raster, tictoc, gdalR)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

# Load modified version of gdal resample, which has target extent as an argument
gdal_resample <- file.path(str_glue("{folder_path}/Code_Global/GDAL/gdal_resample.R"))
source(gdal_resample)

# Load list of files to be aggregated (mosaicked predictions)
files_list <- str_glue("{datafolder_path}/Data_Global/Model_output" ) %>% 
  list.files(pattern = "Prediction_merged_*.*_new.tif$", full.names = TRUE)

# Set spatial resolution
resolution_30s <- 0.00833333333333333
resolution_5min <- 0.0833333333333333
resolution_10min <- 0.166666666666667

# 
my_rezz <- data.frame(
  res_str = c("30s", "5m", "10m"),
  res_num = c(resolution_30s, resolution_5min, resolution_10min),
  stringsAsFactors = FALSE
)

out_res <- pull(my_rezz, "res_str")

methods <- c("average", "med", "max", "min")

# Loop over files, resolutions, and summarizing methods
for (file in files_list)
{
  for (output_resolution in out_res)
  {   
    for (method in methods)
    {
      
      outname <- basename(file) %>% 
        str_replace("_merged_", str_glue("_aggregated_{output_resolution}_{method}_")) %>% 
        str_remove("_new_april")
      
      
      out_rezz <- my_rezz %>% 
        filter(res_str == output_resolution) %>% 
        pull(res_num) %>% 
        as.character()
      
      # print(outname)
      basef <- str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers")
      
      output_name <- str_glue("{basef}/{outname}")
      
      if (!file.exists(output_name))
      {
        tic("Resample projection")
        GDAL_resample2(infile = file, outfile = output_name, target_extent = "-180 -57 180 84",
                       target_resolution = out_rezz, method = method, large_tif = TRUE)
        toc()
      }      
    }
  }
}

