#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=12:00:00
#SBATCH --output "/Logs/DEM_resampling.log"
#SBATCH --mem=48G

##########################################
####  Process DEM
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: mirzaceng@gmail.com
##########################################


# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc, glue, rgdal, sf, gdalR)

# Folder Path -------------------------------------------------------------
scale <- "Global"

local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" 
   datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data" 
}

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
   datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"
}


gdal_resample <- file.path(str_glue("{folder_path}/Code_{scale}/GDAL/gdal_resample.R"))
source(gdal_resample)

Crop_function <- file.path(str_glue("{folder_path}/Code_{scale}/Data_processing/Crop_function.R"))
source(Crop_function)

# Set filenames ---------------------------------------------------------------
# dem_path_raw <- raster("/vol/milkundata/Merit_DEM/Merit_DEM_mosaic.tif") 
dem_norm <- raster(str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif"))

dem_resampled <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/DEM_resample.tif") 

dem_out <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/DEM_fnl.tif") 



# Resample layer
# For Linux
if (!file.exists(dem_resampled))
{
  
  tic("Resample DEM")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif"), outfile = dem_resampled, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "average", large_tif = TRUE)
  toc()
  
}

DEM_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/DEM_resample.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/DEM_fnl.tif"), 
                         scale = scale)

test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/DEM_fnl.tif"))
plot(test)

#