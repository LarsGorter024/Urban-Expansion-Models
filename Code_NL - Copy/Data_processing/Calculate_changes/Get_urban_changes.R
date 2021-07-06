##########################################
####  Get changes between two timesteps
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter 
#### | Contact: mirzaceng@gmail.com
##########################################

# This script will take two land cover rasters, split them in many small parts (in parallel),
# and calculate the differences where land has been converted into urban area

# Load packages -----------------------------------------------------------
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")

pacman::p_load(raster, rgdal, Rahat, tictoc, sf, fs, glue, foreach, tidyverse, pkgmaker, mapview, GSIF, foreach, doParallel, doSNOW, gdalR)

# Folder Path -------------------------------------------------------------
scale <- "NL"
local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" 
datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data" 
}

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"
}

Crop_function <- file.path(str_glue("{folder_path}/Code_NL/Data_processing/Crop_function.R"))
source(Crop_function)


# Crop input rasters ------------------------------------------------------
lc_1993 <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_1993.tif"))
lc_2006 <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2006.tif"))
lc_2019 <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019.tif"))

Crop_function(infile = lc_1993, 
              outfile =  str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_1993_crop.tif"), 
              scale = scale)

Crop_function(infile = lc_2006, 
              outfile =  str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2006_crop.tif"), 
              scale = scale)

Crop_function(infile = lc_2019, 
              outfile =  str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019_crop.tif"), 
              scale = scale)

# Mask to which point data is rarified
bioclim_mask <- raster("/vol/milkundata/Chelsa/CHELSA_bioclim/CHELSA_bio10_1.tif") 
Crop_function(infile = bioclim_mask, 
                              outfile =  str_glue("{datafolder_path}/Data_{scale}/bioclim_rarify_mask.tif"), 
                              scale = scale)

bioclim_mask <- raster(str_glue("{datafolder_path}/Data_{scale}/bioclim_rarify_mask.tif"))
class(bioclim_mask)

# bioclim_mask <- raster("//milkunstud-srv.science.ru.nl/milkundata/Chelsa/CHELSA_bioclim/CHELSA_bio10_1.tif")

# Load data ---------------------------------------------------------------
## Create folders
# This is the folder that will contain the output data
# changes_type <- "fit"
# changes_type <- "eval"
for (changes_type in c("eval", "fit"))
{
  folder_basepath <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/{changes_type}/Presences") 
  dir.create(folder_basepath, recursive = TRUE, showWarnings = FALSE)
  
  # Load data ####
  if (changes_type == "fit")
  {
    mybrick <- stack(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2006_crop.tif"), 
                     str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019_crop.tif"))	
  } 
  if (changes_type == "eval")
  {
    mybrick <- stack(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_1993_crop.tif"),
                     str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2006_crop.tif"))
  }
  
  # test_nocrop <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019.tif"))
  # test_crop <- raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019_crop.tif"))
  # plot(test_crop)
  
  ##%######################################################%##
  #                                                          #
  ####                     Main loop                      ####
  #                                                          #
  ##%######################################################%##
  
  #### Main ####
  
  # Necessary steps
  # - load two rasters
  # - run get change
  # - raster to point
  # - rarify point
  
  
  #### Set output names ####
  # Change raster filepath
  category <- "190"
  
  raster_out_filename <- str_glue("{folder_basepath}/Presence_{tolower(changes_type)}_{as.character(category)}.tif")
  # Change shapefile (not rarified)
  shape_out_filename <- str_glue("{folder_basepath}/Presence_{tolower(changes_type)}_{as.character(category)}_300m.gpkg")
  # Change shapefile (rarified)
  shape_out_rare_filename <- str_glue("{folder_basepath}/Presence_{tolower(changes_type)}_{as.character(category)}_1km.gpkg")
  
  ###########
  # nl_lc_2003_outfile <- str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2003.tif")
  # nl_lc_2013_outfile <- str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2013.tif")
  
  #### Get change from two rasters ####
  # Check if raster exists and substract two raster if not
  registerDoParallel(cores = 20)
  n_cores <- 20  # 4 for local
  
  get_change_raster_GDAL <- file.path(str_glue("{folder_path}/Code_NL/GDAL/get_change_raster_GDAL.R"))
  source(get_change_raster_GDAL)
  
  # if (!file.exists(raster_out_filename))
  # {
  print("Getting change.")
  tic(glue("Getting change for {changes_type} {category}."))
  
  category <- as.numeric(category)
  change_raster_temp <- get_change_raster_GDAL(x = mybrick[[1]], y = mybrick[[2]],
                                               outpath = paste0("/scratch/R_temp_mosaic_", tolower(changes_type), category),
                                               outfile = raster_out_filename,
                                               category = "190", number_of_cores = n_cores)
  # writeRaster(change_raster_temp, outfile= str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/{changes_type}/Presences/Change_raster_temp_{changes_type}.tif"))
 
  raster_out_filename <- raster(str_glue("{folder_basepath}/Presence_{tolower(changes_type)}_190.tif"))
  raster_out_filename[lc_2006 == 210] <- NA
  
 
  
  # Crop_function(infile = raster_out_filename, 
  #               outfile =  str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/{changes_type}/Presences/Change_raster_{changes_type}.tif"), 
  #               scale = "NL")
  
  # change_raster <-  raster(str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_vector/{changes_type}/Presences/Change_raster_{changes_type}.tif"))
  
  change_raster <- raster_out_filename
  
  toc()
  
  # } else {
  #   # Load raster otherwise
  #   change_raster <- raster(raster_out_filename)
  # }
  
  # plot(change_raster)
  # mapview(change_raster)
  
  #### Get change shapefile ####
  tic("Raster to points")
  change_points <- raster::rasterToPoints(change_raster, sp = TRUE, fun = function(x){x == 1})   
  toc()
  
  # plot(change_points)
  change_points_sf <- st_as_sf(change_points)
  st_write(change_points_sf, shape_out_filename, append=FALSE, delete_dsn=TRUE)
  # mapview(change_points_sf)

  
  
  #### Rarify points ####
  
  rarify_points <- file.path(str_glue("{folder_path}/Code_NL/rarify_points.R"))
  source(rarify_points)
  
  
  if (!file.exists(shape_out_rare_filename))
  {
    cat(paste0("Rarifying category ", category), "\n")
    change_points_1km <- as(change_points_sf, "Spatial")
    toc("Rarifying...")
    my_urban_rarified <- rarify_points(change_points_1km, bioclim_mask)
    st_write(my_urban_rarified, shape_out_rare_filename)
    toc()
  } else {
    my_urban_rarified <- st_read(shape_out_rare_filename)
  }
}
# summary(my_urban_rarified)
# mapview(my_urban_rarified)

# plot(raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019_crop.tif")))

# shape_out <- st_read(str_glue("{folder_basepath}/Presence_{changes_type}_{category}_300m.gpkg"))
# mapview(shape_out)

# shape_out_1km <- st_read(str_glue("{folder_basepath}/Presence_{changes_type}_{category}_1km.gpkg"))
# mapview(shape_out_1km)

# test <- extract(raster(str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2006_crop.tif")),shape_out_1km)
# head(test)
# unique(test)
# table(test)
