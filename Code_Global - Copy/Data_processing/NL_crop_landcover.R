# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, tictoc, janitor, raster, sf, mapview, ncdf4, gdalUtils,rgdal)

# Folder Path -------------------------------------------------------------

local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" }

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" }


#### Load NL data
nl_outpath <- "~/Studie/Master/MSc STAGE/NL_bound.gpkg" 

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

# Crop processing mask to processing extent
nl_lc_1992_infile <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7.tif"
nl_lc_1993_infile <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1993-v2.0.7.tif"
nl_lc_2005_infile <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7.tif"
nl_lc_2006_infile <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2006-v2.0.7.tif"
nl_lc_2019_infile <- "//milkunstud-srv.science.ru.nl/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.nc"

###
nl_lc_1992_outfile <- str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_1992.tif") 
nl_lc_1993_outfile <- str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_1993.tif")
nl_lc_2005_outfile <- str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_2005.tif")
nl_lc_2006_outfile <- str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_2006.tif")
nl_lc_2019_outfile <- str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_2019.tif")

file.path <- str_glue("{folder_path}/Data/Data_NL/NL_data")

#### Crop files
# # For LINUX
# if (!file.exists(nl_lc_1993_outfile))
# {
  # tic("Cropping landcover")
  # gdalR::GDAL_crop(nl_lc_1993_infile, nl_lc_1993_outfile, shapefile_path = nl_outpath,
  #                  large_tif = TRUE)
  # toc()
# }

# For 1992
crop_nl_1992 <- crop(raster(nl_lc_1992_infile),nl_bound)
nl_lc_1992_outfile <- mask(crop_nl_1992, nl_bound)
plot(nl_lc_1992_outfile)
writeRaster(nl_lc_1992_outfile, filename = "NL_landcover_1992",format="GTiff")


# For 1993
crop_nl_1993 <- crop(raster(nl_lc_1993_infile),nl_bound)
nl_lc_1993_outfile <- mask(crop_nl_1993, nl_bound)
plot(nl_lc_1993_outfile)
writeRaster(nl_lc_1993_outfile, filename = "NL_landcover_1993",format="GTiff")

# Check
nl_1993 <- raster(str_glue("{folder_path}/Data/Data_NL/NL_data/NL_landcover_1993.tif"))
plot(nl_1993)

# For 2005
crop_nl_2005 <- crop(raster(nl_lc_2005_infile),nl_bound)
nl_lc_2005_outfile <- mask(crop_nl_2005, nl_bound)
plot(nl_lc_2005_outfile)
writeRaster(nl_lc_2005_outfile, filename = "NL_landcover_2005",format="GTiff")


# For 2006
crop_nl_2006 <- crop(raster(nl_lc_2006_infile),nl_bound)
nl_lc_2006_outfile <- mask(crop_nl_2006, nl_bound)
plot(nl_lc_2006_outfile)
writeRaster(nl_lc_2006_outfile, filename = "NL_landcover_2006",format="GTiff")


# For 2019
crop_nl_2019 <- crop(raster(nl_lc_2019_infile),nl_bound)
nl_lc_2019_outfile <- mask(crop_nl_2019, nl_bound)
plot(nl_lc_2019_outfile)
writeRaster(nl_lc_2019_outfile, filename = "NL_landcover_2019",format="GTiff")


# out_mask_filename_rcl <- "Projects/Agriculture_modeling/Data/Data_NL/processing_mask_cropped_binary.tif" %>% 
#   milkunize2("archive")