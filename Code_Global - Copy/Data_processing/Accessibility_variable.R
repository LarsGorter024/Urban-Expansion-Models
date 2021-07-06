##########################################
####  Process Accessibility
##########################################
#### | Project name: Urban modeling
#### | Creator: Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Script setup ------------------------------------------------------------
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc, glue, rgdal, sf)


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
# File 12 has all the relevant sizes of cities
# access_raw <- raster(str_glue("{datafolder_path}/Predictors/Accessibility/7638134/travel_time_to_cities_12.tif"))
access_norm <- raster(str_glue("{datafolder_path}/Predictors/Accessibility/Access_norm.tif"))
# acces_fnl <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Access_fnl.tif"))

# writeRaster(access_raw, filename = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/access_raw.tif") ,format="GTiff", overwrite=TRUE)



# Log transformation ------------------------------------------------------

# Access_log <- raster(str_glue("{datafolder_path}/Predictors/Accessibility/Access_log.tif"))

# if (!file.exists(Access_log))
# {
#   logtr_command <- glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/Accessibility/access_no_water_crop.tif --calc \"log10(A+1)\" --type Float32 --outfile {Access_log}")
#   print("Log transforming access")
#   system(logtr_command)
#   
#   
# }



# Resample layer ----------------------------------------------------------

Access_resampled <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Access_resampled.tif")

# Access_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Access_fnl.tif")
# Access_fnl <- str_glue("{datafolder_path}/Predictors/Accessibility/Access_fnl.tif")



# For Linux
if (!file.exists(Access_resampled))
{

  tic("Resample access")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Accessibility/Access_norm.tif"), outfile = Access_resampled, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
  toc()

}

Access_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Access_resampled.tif"),
                            outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Access_fnl.tif"),
                            scale = scale)

# test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Access_fnl.tif"))
# plot(test)
