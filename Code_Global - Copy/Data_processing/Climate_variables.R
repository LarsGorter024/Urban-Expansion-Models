##########################################
####  Prepare climate variables
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Steps:
# Load raw chelsa climate data
# log transform one of the layers (12)
# Normalize data in python
# Resample to target resolution and extent

# Load packages ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, janitor, tictoc, glue, RStoolbox, raster, sf, gdalR)


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


# Load raw CHELSA rasters, layers 01, 04  (mean temperature, mean precipitation)

meantemp <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/Normalized/Temperature_norm.tif"
meanprecip <- "/vol/milkundata/Chelsa/CHELSA_bioclim/CHELSA_bio10_4.tif" 


temperature <- raster(meantemp)
precipitation <- raster(meanprecip)

# logtransform the precipitation data -------------------------------------
# Variable 12 is skewed (precipitation)
in_grid <- precipitation
writeRaster(precipitation, filename = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Precip_temp.tif") , format="GTiff", overwrite=TRUE)


out_grid <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Precip_log.tif") 
out_grid_temp <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Temperature.tif") 


# For Linux
if (!file.exists(out_grid))
{
  logtr_command <- glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_{scale}/Predictors_intermediate/Precip_temp.tif --calc \"log10(A+1)\" --type Float32 --outfile {out_grid}")
  print("Log transforming precip")
  system(logtr_command)
  
}

writeRaster(temperature, filename = out_grid_temp,format="GTiff", overwrite=TRUE)



## Centering is done in Python


# Resample layers ---------------------------------------------------------

# For Linux
Precipitation_resampled <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Precipitation_resampled.tif")
Temperature_resampled <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Temperature_resampled.tif")

Precipitation_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Precipitation_fnl.tif")
Temperature_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Temperature_fnl.tif")

# Resample layers
if (!file.exists(Precipitation_resampled))
{
  tic("Resampling Precipitation")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Normalized/precip_norm.tif") , outfile = Precipitation_resampled, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
  toc()
  
}

if (!file.exists(Temperature_resampled))
{
  tic("Resampling Temperature")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Temperature.tif") , outfile = Temperature_resampled, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)   
  toc()
}

Precipitation_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Precipitation_resampled.tif"), 
                                   outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Precipitation_fnl.tif"), 
                                   scale = scale)

Temperature_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Temperature_resampled.tif"), 
                                 outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Temperature_fnl.tif"), 
                                 scale = scale)


# test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Precipitation_fnl.tif"))
# plot(test, zlim=c(-3,3))
# test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Temperature_fnl.tif"))
# plot(test, zlim=c(-1,3))

#### FIN ####
