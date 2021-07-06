##########################################
####  Calculate water distance
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################


# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, glue, tictoc, rgdal,pkgmaker)


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

Crop_function <- file.path(str_glue("{folder_path}/Code_{scale}/Data_processing/Crop_function.R"))
source(Crop_function)

# Define function ---------------------------------------------------------

# infile - raster for which to calculate distances
# outfile - output filename
# to_tiff - convert .sdat file to .tiff; default is FALSE

SAGA_distance <- function(infile, outfile, to_tiff = TRUE)
{
  
  if(pkgmaker::file_extension(outfile) == "tif")
  {
    outfile <- gsub(".tif", ".sgrd", outfile)
  }
  
  saga_call <- glue::glue("saga_cmd grid_tools 26 -FEATURES {infile} -DISTANCE {outfile}")
  system(saga_call)
  
  gdal_call <-   glue("gdal_proximity.py {infile} {outfile}")
  # system(gdal_call)
  
  if (isTRUE(to_tiff))
  {
    system(paste0("gdal_translate -of GTiff", " ", gsub("sgrd", "sdat", pkgmaker::file_extension(outfile)), " ",  gsub("sgrd", "tif", pkgmaker::file_extension(outfile))))
  }
}


# Define filenames ---------------------------------------------------------------

# Input
esa_path <- "/vol/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7.tif"
water_2005 <- raster(esa_path)

# Reclassified
outname_210 <- str_glue("{datafolder_path}/Data_Global/Predictors_intermediate/ESA_210.tif")


# Distance names
outname_210_distance <- str_glue("{datafolder_path}/Data_Global/Predictors_intermediate/ESA_210_distance.tif")

# 
# 
# # Reclassify land cover rasters -------------------------------------------
# # For Linux
# string_esa210 <- glue::glue("gdal_calc.py -A {esa_path} --outfile={outname_210} --calc=\"A*(A==210)\" --NoDataValue=0")
# 
# if (!file.exists(outname_210))
# {
#   tic("Reclassifying 210")
#   system(string_esa210)
#   toc()
# 
# }
# 
# 
# # Reproject to equal area -------------------------------------------------
# 
# 
# GDAL_reproject <- function(input, outfile, crs_target, method, return_raster = FALSE)
# {
#   if (!method %in% c("near", "bilinear", "cubic", "cubicspline", "lanczos",
#                      "average", "mode", "max", "min", "med", "q1", "q3")) {
#     stop("Resampling method not available.")
#   }
# 
#   proj.cmd.warp <- paste0("gdalwarp -t_srs", " ", "'",
#                           crs_target,"'" , " ","-r", " ", method, " ", "-of vrt")
# 
#   print(paste(proj.cmd.warp, input, gsub(pkgmaker::file_extension(outfile), "vrt", outfile)))
#   # Reproject to vrt in order to conserve space
#   system(command = paste(proj.cmd.warp, input, gsub(pkgmaker::file_extension(outfile), "vrt", outfile)))
#   # Load and transform to tiff
#   system(paste("gdal_translate -co compress=LZW", gsub(pkgmaker::file_extension(outfile), "vrt", outfile),
#                outfile))
#   # Remove vrt file
#   unlink(gsub(pkgmaker::file_extension(outfile), "vrt", outfile))
# 
#   # Return raster
#   if (isTRUE(return_raster)) {
#     library(raster)
#     out <-raster(outfile)
#     return(out)
#   }
# }
# 
# 
# outname_210_ea <- str_glue("{datafolder_path}/Data_Global/Predictors_intermediate/ESA_210_eqa2.tif")
# 
# proj_ed <- "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# 
# # plot(rr)
# if (!file.exists(outname_210_ea))
# {
#   tic("Reprojecting 210")
#   GDAL_reproject(
#     outname_210,
#                  # esa_path,
#                  outname_210_ea, crs_target = proj_ed, method = "bilinear")
#   toc()
# 
# }
# 
# outname_210_distance2 <- str_replace(outname_210_distance, "e.tif", "e2.tif")
# 
# if (!file.exists(outname_210_distance2))
# {
#   tic("Calculating distance 210")
#   SAGA_distance(outname_210_ea, outname_210_distance2)
#   toc()
# 
# }
# 
# 
# # Calculate distance ------------------------------------------------------
# if (!file.exists(outname_210_distance))
# {
#   tic("Calculating distance 210")
#   SAGA_distance(outname_210, outname_210_distance)
#   toc()
#   
# }

# ESA_210_distance_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/ESA_210_distance_fnl.tif")
# 
e <- extent(raster(str_glue("{datafolder_path}/Predictors/Normalized/Merit_DEM_normalized.tif")))
dist <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance.tif"))
dist_crop_extent <- crop(dist,e)
writeRaster(dist_crop_extent, filename = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance_crop.tif"), format="GTiff", overwrite=TRUE)


ESA_210_distance_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance_crop.tif"),
                                      outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/ESA_210_distance_fnl.tif"),
                                      scale = scale)

# ESA_210_distance2_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/ESA_210_distance2_fnl.tif")
# 
# ESA_210_distance2_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/ESA_210_distance2.tif"), 
#                                       outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/ESA_210_distance2_fnl.tif"), 
#                                       scale = scale)

# ESA_210_test <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/ESA_210_distance_fnl.tif"))
# plot(ESA_210_test)



#
