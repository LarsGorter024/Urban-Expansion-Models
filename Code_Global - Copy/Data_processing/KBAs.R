##########################################
####  Create predictor - key biodiversity areas
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Steps:
# 1 - Filter area polygons to keep only what we need.
# 2 - Rasterize on a global level and save

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, raster, sf, tidyverse, fasterize, tictoc, rgdal, mapview)


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

# Load data ---------------------------------------------------------------

KBAs_raw <- str_glue("{datafolder_path}/Predictors/KBAs/KBAsGlobal_2020_September_02_POL.shp") %>%
  st_read()

KBAs <- KBAs_raw  %>% # Potential status: confirmed, de-listed, candidate, superseded, proposed & does not qualify
  filter(is.na(KbaStatus)| KbaStatus != "de-listed") %>%
  filter(is.na(KbaStatus)| KbaStatus != "superseded") %>%
  filter(is.na(KbaStatus)| KbaStatus != "does not qualify")

unique(KBAs$KbaStatus)


# Load ESA land cover mask. This will be used as a raster mask to rasterize the polygon data.
mask_2019 <- raster("/vol/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.tif")

rasterOptions(maxmemory = ncell(mask_2019) - 1)

# Rasterize data so background is 0, and protected is 1

KBAs_filename <- str_glue("{datafolder_path}/Data_Global/Predictors_intermediate/KBAs.tif") 


if (!file.exists(KBAs_filename))
{
  
  tic("Rasterizing")
  KBAs_raster <- fasterize(KBAs, mask_2019, fun = "first", background = 0)
  toc()
  
  # KBAs_raster[is.na(KBAs_raster)] <- 0
  
  writeRaster(KBAs_raster, KBAs_filename, options = "COMPRESS=LZW")  
}
# plot(KBA_raster)

##### Harmonize
# Load & Resample data ---------------------------------------------------------------
KBAs_filename_resamp <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/KBAs_resamp.tif") 

KBA_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/KBAs_fnl.tif") 


if (!file.exists(KBAs_filename_resamp))
{
  
  tic("Harmonizing PAs")
  GDAL_resample2(infile = KBAs_filename, outfile = KBAs_filename_resamp, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "near", large_tif = TRUE)
  toc()
  
}

KBA_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/KBAs_resamp.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/KBAs_fnl.tif"), 
                         scale = scale)

# KBA_test <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/KBAs_fnl.tif"))
# plot(KBA_test)


#