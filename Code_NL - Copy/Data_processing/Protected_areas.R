##########################################
####  Create predictor - protected areas & proposed protected areas
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Steps:
# 1 - Filter protected areas polygons to keep only what we need.
# 2 - Rasterize on a global level and save

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, raster, sf, tidyverse, fasterize, tictoc, rgdal)

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

# Load data ---------------------------------------------------------------

p_areas1 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp0/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
st_read()

p_areas2 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp1/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
  st_read()

p_areas3 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp2/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
  st_read()

PAs <- rbind(p_areas1, p_areas2, p_areas3)


protected_areas <- PAs %>%
  filter(MARINE == 0)  %>%
  filter(STATUS != "Proposed")

proposed_areas <- PAs %>%
  filter(MARINE == 0)  %>%
  filter(STATUS == "Proposed")



# Load ESA land cover mask. This will be used as a raster mask to rasteriye the polygon data.
mask_nl_2019 <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019.tif"))

rasterOptions(maxmemory = ncell(mask_nl_2019) - 1)

# Rasterize data so background is 0, and protected is 1
# Load data 

protected_filename <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Protected_areas.tif") 
proposed_protected_filename <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Proposed_protected_areas.tif") 


if (!file.exists(protected_filename))
{
  
  tic("Rasterizing")
  pa_protected <- fasterize(protected_areas, mask_nl_2019, fun = "first", background = 0)
  toc()
  # pa_protected[is.na(pa_protected)] <- 0 
  
  writeRaster(pa_protected, protected_filename, options = "COMPRESS=LZW", format="GTiff", overwrite=TRUE)  
}


if (!file.exists(proposed_protected_filename))
{
  
  tic("Rasterizing")
  pa_proposed <- fasterize(proposed_areas, mask_nl_2019, fun = "first", background = 0)
  toc()
  # pa_proposed[is.na(pa_proposed)] <- 0
  
  writeRaster(pa_proposed, proposed_protected_filename, options = "COMPRESS=LZW")  
}

pa_resamp_filename <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Protected_areas_resamp.tif") 
proppa_resamp_filename <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Proposed_protected_resamp.tif")

pa_filename_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Protected_areas_fnl.tif") 
proppa_filename_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Proposed_protected_fnl.tif")


# Resample layers ---------------------------------------------------------

if (!file.exists(pa_resamp_filename))
{

  tic("Harmonizing PAs")
  GDAL_resample2(infile = protected_filename, outfile = pa_resamp_filename, target_extent = "3.360782 50.723492  7.227095 53.554584",
                 target_resolution = "0.002777777777778", method = "near", large_tif = TRUE)
  toc()

}

if (!file.exists(proppa_resamp_filename))
{
  
  tic("Harmonizing PAs")
  GDAL_resample2(infile = proposed_protected_filename, outfile = proppa_resamp_filename, target_extent = "3.360782 50.723492  7.227095 53.554584",
                 target_resolution = "0.002777777777778", method = "near", large_tif = TRUE)
  toc()
  
}

pa_filename_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Protected_areas_resamp.tif"), 
                              outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Protected_areas_fnl.tif"), 
                              scale = "NL")

proppa_filename_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Proposed_protected_resamp.tif"), 
                              outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Proposed_protected_fnl.tif"), 
                              scale = "NL")


# pa_test <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Protected_areas_fnl.tif"))
# plot(pa_test)
# class(pa_test)

# proppa_test <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Proposed_protected_fnl.tif"))
# freq(proppa_test)
