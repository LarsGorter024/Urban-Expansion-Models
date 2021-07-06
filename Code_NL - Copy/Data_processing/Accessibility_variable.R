##########################################
####  Process Accessibility
##########################################
#### | Project name: Urban modeling
#### | Creator: Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc, glue, rgdal, sf)


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

# Set filenames ---------------------------------------------------------------
# File 12 has all the relevant sizes of cities
# access_raw <- str_glue("{datafolder_path}/Predictors/Accessibility/7638134/travel_time_to_cities_12.tif") 
access_norm <- str_glue("{datafolder_path}/Predictors/Accessibility/Access_norm.tif")
# extent(raster(access_norm))
Access_resampled <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Access_resampled.tif")

Access_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Access_fnl.tif")
plot(raster(Access_fnl))

# esa <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.tif" 
# esa <- "/vol/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.tif"
# esa_rast <- raster(esa)
# extent(esa_rast)
# esa_mask_crop <- crop(esa_rast,e)
# writeRaster(esa_mask_crop, filename = str_glue("{datafolder_path}/Data_Global/Crop_mask_esa_2019.tif"), format="GTiff", overwrite=TRUE)
# 
# e <- extent(raster(str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif")))
# access_crop <- crop(raster(access_raw),e)
# writeRaster(access_crop, filename = str_glue("{datafolder_path}/Predictors/Accessibility/Access_crop.tif"), format="GTiff", overwrite=TRUE)

# NL Crop -----------------------------------------------------------------
nl_outpath <- str_glue("{folder_path}/NL_bound.gpkg") 

nl_bound <- st_read(nl_outpath)


nl_access <- crop(raster(access_norm),nl_bound)
nl_access <- mask(nl_access, nl_bound)
# plot(nl_access)
# nl_access[is.na(nl_access)] <- 0 


writeRaster(nl_access, filename = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/nl_access.tif"),format="GTiff", overwrite=TRUE)
class(nl_access)
print("Resampling")



# Resample layer ----------------------------------------------------------
# For Linux
if (!file.exists(Access_resampled))
{
  
  tic("Resample access")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/nl_access.tif"), outfile = Access_resampled, target_extent = "3.360782 50.723492  7.227095 53.554584",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
  toc()
  
}

Access_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Access_resampled.tif"), 
                            outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Access_fnl.tif"), 
                            scale = "NL")

# test_access <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Access_fnl.tif"))
# plot(test_access)
# plot(!is.na(test_access))
# freq(test_access)

# Access_resampled <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Access_resampled.tif"))
# plot(Access_resampled)

# Fitted_model_ANN_eval_190 <- readRDS("//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_NL/Model_output/Model_runs/Fitted_model_ANN_eval_190.rds")
