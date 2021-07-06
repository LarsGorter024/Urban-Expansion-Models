#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=6:00:00
#SBATCH --output "Logs/Extract_grid_vals.out"
#SBATCH --mem=20G

##########################################
####  Create shapefile of presences + absences
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: mirzaceng@gmail.com
##########################################

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, sf, tictoc,dplyr)


# Folder Path -------------------------------------------------------------


local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" 
   datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data" 
}

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
   datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"
}


# Define function that uses SAGA GIS (quickest algorithm)
raster_to_point_SAGA2 <- function(in_raster, in_shape, out_shape)
{
  sys_call <- stringr::str_glue("saga_cmd shapes_grid 0 -GRIDS:{in_raster} -SHAPES:{in_shape} -RESULT:{out_shape} -RESAMPLING:0")  
  tictoc::tic("Extracting raster values")
  system(sys_call)
  tictoc::toc()
}

# Run stuff ---------------------------------------------------------------

# Get raster mask

grid_mask_filename <-  str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019.tif")
grid_mask <- raster(grid_mask_filename)


# Define folder in which the outputs will be stored
output_folder <- str_glue("{datafolder_path}/Data_NL/Response_variable") 

#### Loop over categories and models
category <- "190"

for (type in c("fit", "eval"))
{
  category_id <- str_glue("{type}_{category}")
  # Define output name for the cleaned geopackage file of the response variable (presences and absences combined)
  urbanchanges_filename <- str_glue("{output_folder}/Changes_vector/Urbanchanges_{category_id}_ppa.gpkg")
  
  if (!file.exists(urbanchanges_filename))
  {
    
    # Get rarified presences
    
    presence_files <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/{type}/Presences") %>% 
      list.files(recursive = TRUE, pattern = "1km.gpkg", full.names = TRUE) 
    
    
    # Get rarified global absences (PPA = number of presences equal to absences)
    absence_files <- str_glue("{datafolder_path}/Data_NL/Absences/{type}/Absences_{type}.gpkg") 
    
    
    absences_sf <- st_read(absence_files) %>%
      transmute(
        PA = 0
      )
    
    print(absences_sf)
    presences_sf <- st_read(presence_files) %>% 
      transmute(
        PA = 1
      )
    
    # Get the number of presences and absences
    # If the number is not equal, then subset which ever one there's more and combine into PPA dataset.
    absences_num <- nrow(absences_sf)
    presences_num <- nrow(presences_sf)
    
    if (!identical(presences_num, absences_num))
    {
      if (presences_num > absences_num)
      {
        presences_sampled <- dplyr::sample_n(presences_sf, absences_num)
        # Combine presences and absences 
        urbanchanges_sf <- rbind(presences_sampled, absences_sf)
        st_write(urbanchanges_sf, urbanchanges_filename)
      }
      if (absences_num > presences_num)
      {
        absences_sampled <- dplyr::sample_n(absences_sf, presences_num)
        # Combine presences and absences 
        urbanchanges_sf <- rbind(presences_sf, absences_sampled)
        st_write(urbanchanges_sf, urbanchanges_filename)
      }
      
    } else 
    {
      # Combine presences and absences 
      urbanchanges_sf <- rbind(presences_sf, absences_sf)
      st_write(urbanchanges_sf, urbanchanges_filename)
    }
  }
  
  
  # Use SAGA to extract the value of grid ----
  
  # Define output filename for extracted values in presences and absences
  # 
  output_file <- str_glue("{output_folder}/Changes_vector/Urbanchanges_{category_id}_ppa_grids.shp")
  
  if (!file.exists(output_file))
  {
    raster_to_point_SAGA2(in_raster = grid_mask_filename, in_shape = urbanchanges_filename, out_shape = output_file)
  }
  
}

