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
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, janitor, tictoc, glue, RStoolbox, raster, sf, gdalR)


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

gdal_resample <- file.path(str_glue("{folder_path}/Code_NL/GDAL/gdal_resample.R"))
source(gdal_resample)

Crop_function <- file.path(str_glue("{folder_path}/Code_NL/Data_processing/Crop_function.R"))
source(Crop_function)

# Load raw CHELSA rasters, layers 01, 04  (mean temperature, mean precipitation)

meanprecip <- "/vol/milkundata/Chelsa/CHELSA_bioclim/CHELSA_bio10_4.tif" 
precipitation <- raster(meanprecip)

# meantemp <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/Normalized/Temperature_norm.tif" 
# rawtemp <- "/vol/milkundata/Chelsa/CHELSA_bioclim/CHELSA_bio10_1.tif" 
meantemp <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/Normalized/Temperature_norm.tif"
temperature <- raster(meantemp)
summary(temperature)

# e <- extent(raster(str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif")))
# 
# dist <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance.tif"))
# dist_crop_extent <- crop(dist,e)
# writeRaster(dist_crop_extent, filename = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance_crop.tif"), format="GTiff", overwrite=TRUE)

 

# NL Crop -----------------------------------------------------------------

nl_outpath <- str_glue("{folder_path}/NL_bound.gpkg") 

if (!file.exists(nl_outpath))
{
  print("Loading file")
  nl_bound <- getData("GADM", country = "NL", level = 0) %>% 
    st_as_sf()
  
  st_write(nl_bound, nl_outpath)
} else {
  print("file exists")
  nl_bound <- st_read(nl_outpath)
}

# nl_test_precip_norm <- crop(test_precip, nl_bound)
# writeRaster(nl_test_precip_norm, filename=str_glue("{datafolder_path}/Predictors/Normalized/precip_NL_norm.tif"), format="GTiff", overwrite=TRUE) 

nl_temp <- crop(temperature, nl_bound)

nl_precip <- crop(precipitation,nl_bound)


# logtransform the precipitation data -------------------------------------
in_grid <- nl_precip
writeRaster(nl_precip, filename = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Precip_temp.tif") , format="GTiff", overwrite=TRUE)

out_grid <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Precip_log.tif") 
out_grid_temp <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Temperature.tif") 

# For Linux
if (!file.exists(out_grid))
{
  logtr_command <- glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_NL/Predictors_intermediate/Precip_temp.tif --calc \"log10(A+1)\" --type Float32 --outfile {out_grid}")
  print("Log transforming precip")
  system(logtr_command)
  

}

writeRaster(nl_temp, filename = out_grid_temp,format="GTiff", overwrite=TRUE)


## Centering is done in Python
Precipitation_resampled <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Precipitation_resampled.tif")
Temperature_resampled <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Temperature_resampled.tif")

Precipitation_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Precipitation_fnl.tif")
Temperature_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Temperature_fnl.tif")

# Resample layers ---------------------------------------------------------
# For Linux
if (!file.exists(Precipitation_resampled))
{
  tic("Resampling Precipitation")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Normalized/precip_NL_norm.tif") , outfile = Precipitation_resampled, target_extent = "3.360782 50.723492  7.227095 53.554584",
                  target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
  toc()
   
}

if (!file.exists(Temperature_resampled))
{
  tic("Resampling Temperature")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Temperature.tif") , outfile = Temperature_resampled, target_extent = "3.360782 50.723492  7.227095 53.554584",
                  target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)   
  toc()
}

Precipitation_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Precipitation_resampled.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Precipitation_fnl.tif"), 
                         scale = "NL")

Temperature_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Temperature_resampled.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Temperature_fnl.tif"), 
                         scale = "NL")

# test_precip <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Precipitation_fnl.tif"))
# plot(test_precip)
# plot(!is.na(test_precip))

# test_temp <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Temperature_fnl.tif"))
# plot(test_temp)
# plot(!is.na(test_temp))


########################################################
#### FIN ####
