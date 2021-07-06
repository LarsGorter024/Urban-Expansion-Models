##########################################
####  Extract data from rasters
##########################################
#### | Project name: Urban modeling 
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, sf, tictoc)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"


# Define function
raster_to_point_SAGA2 <- function(in_raster, in_shape, out_shape)
{
  sys_call <- stringr::str_glue("saga_cmd shapes_grid 0 -GRIDS:{in_raster} -SHAPES:{in_shape} -RESULT:{out_shape} -RESAMPLING:0")  
  tictoc::tic("Extracting raster values")
  system(sys_call)
  tictoc::toc()
}

# Run stuff ---------------------------------------------------------------
i <- as.numeric(commandArgs(trailingOnly = TRUE))
category <- "190"

# Get predictor list ------------------------------------------------------

predictors_list <- str_glue("{datafolder_path}/Data_NL/Predictors_final") %>%
  list.files(full.names = TRUE)

# Define folder in which the outputs will be stored
output_folder <- str_glue("{datafolder_path}/Data_NL/Response_variable") 

# Loop
for (type in c("fit", "eval"))
{
  
  if (type == "fit") 
  {
    type2 = "eval"
  } 
  if (type == "eval")
  {
    type2 = "fit"
  }
  
  
  predictors_list_clean <- predictors_list %>%
    str_subset(type2, negate = TRUE) %>% 
    str_subset(pattern = ".tif")
  
  my_predictor_file <- predictors_list_clean[i]
  
  out_layer_name <- predictors_list_clean[i] %>%
    str_remove(str_glue("{datafolder_path}/Data_NL/Predictors_final/")) %>%
    str_remove("_fnl.tif")
  
  category_id <- str_glue("{type}_{category}")
  
  # Define input name for the cleaned geopackage file of the response variable (presences and absences combined)
  #
  urbanchanges_path <- str_glue("{output_folder}/Changes_vector/Urbanchanges_{category_id}_ppa_grids.shp")
  
  # Use SAGA to extract the values of rasters ----
  
  # Define output filename for extracted values in presences and absences
  output_file <- str_glue("{output_folder}/Changes_vector/Single_files/Urbanchanges_{category_id}_{out_layer_name}_extracted_ppa_grids.shp")
  
  if (!file.exists(output_file))
  {
    tic("saga")
    raster_to_point_SAGA2(in_raster = my_predictor_file, in_shape = urbanchanges_path, out_shape = output_file)
    toc()
  }
  
}

