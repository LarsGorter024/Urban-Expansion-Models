#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=6:00:00
#SBATCH -o "Logs/extracted_to_csv.out"
#SBATCH --mem=32G


##########################################
####  Convert extracted values of explanatory variables to csv
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# This script takes the output of Changes_calc/Extract_raster_values.R script, 
# converts the values to csv, and combines those csv's into a single file per category and evaluation type.

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(raster, sp, sf, caret, Rahat, ranger, mapview, scrubr,caret, janitor,
               tidyr, tictoc, PresenceAbsence, tidyverse, fs) 

# Load data ---------------------------------------------------------------

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

# Updated the folder in which the files with extracted values are located.

# Input files
extracted_files_list <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/Single_files") %>% 
  list.files(pattern = "Urbanchanges.*ppa_grids.shp$", recursive = TRUE, full.names = TRUE)


####
# Folder for extracted files (output folder)
folder_extracted <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_extracted")

for (file in rev(extracted_files_list))
{
  layer_name_temp <- file %>% 
    str_remove(str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/Single_files/")) %>% 
    str_remove("_extracted_ppa_grids.shp") %>%
    str_remove("Urbanchanges_") %>% 
    str_remove(".tif")
  
  layer_type <- layer_name_temp %>% 
    as_tibble() %>% 
    separate(
      value,
      into = c("type", "category"), sep = "_"
    )
  
  # Get the name of the layer, without the category type
  layer_name <- layer_name_temp %>% 
    str_remove("fit_") %>%
    str_remove("eval_") %>% 
    str_sub(start = 4, end = nchar(layer_name_temp))
  

  output_file_name <- str_glue("{folder_extracted}/Single_files_new/Urbanchanges_{layer_type$type}_{layer_type$category}_{layer_name}_extracted_ppa_grids.csv")
  
  if (!file.exists(output_file_name))
  {
    
      my_file <- file %>%
        st_read()  
  
    my_file_df <- my_file %>%
      st_set_geometry(NULL)
    
    names(my_file_df)[2] <- "grid"
    names(my_file_df)[3] <- layer_name
    data.table::fwrite(my_file_df, output_file_name)
    print(str_glue("Saving {layer_type$type}_{layer_type$category}_{layer_name}"))

  } else {
    print(str_glue("File exists for {layer_type$type}_{layer_type$category}_{layer_name}"))
    
  }
  
}
