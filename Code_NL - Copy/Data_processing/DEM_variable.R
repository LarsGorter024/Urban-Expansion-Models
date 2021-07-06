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
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc, glue, rgdal, sf, gdalR)

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
# dem_path_raw <- raster("//milkunstud-srv.science.ru.nl/milkundata/Merit_DEM/Merit_DEM_mosaic.tif")
# dem_path_raw <- raster("/vol/milkundata/Merit_DEM/Merit_DEM_mosaic.tif") 
dem_raww <- str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif")
dem_norm <- raster(dem_raww)
print("1")
dem_resampled <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/DEM_resample.tif") 

dem_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/DEM_fnl.tif") 

# NL Crop
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

nl_dem_raw <- crop(dem_norm,nl_bound)
# nl_dem_raw <- mask(nl_dem_raw, nl_bound)
# plot(nl_dem_raw)
# nl_dem_raw[is.na(nl_dem_raw)] <- 0 
# sum(is.na(nl_dem_raw))
writeRaster(nl_dem_raw, filename = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/DEM_NL_raw.tif"),format="GTiff", overwrite=TRUE)
# class(nl_dem_raw)


# Resample layer
# For Linux
if (!file.exists(dem_resampled))
{

  tic("Resample DEM")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/DEM_NL_raw.tif"), outfile = dem_resampled, target_extent = "3.360782 50.723492  7.227095 53.554584",
                 target_resolution = "0.002777777777778", method = "average", large_tif = TRUE)
  toc()

}


DEM_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/DEM_resample.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/DEM_fnl.tif"), 
                         scale = "NL")

dem_fnl <- raster(dem_fnl)
plot(dem_fnl)
# freq(dem_fnl)
# plot(!is.na(dem_fnl))
# summary(dem_fnl)


# dem_resampled_gdal <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/DEM_fnl.tif"))
# plot(dem_resampled_gdal)

# freq(dem_resampled_gdal, report.nas=TRUE)

