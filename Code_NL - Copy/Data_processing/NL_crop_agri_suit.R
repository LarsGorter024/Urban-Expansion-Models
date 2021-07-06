# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")

.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, tictoc, janitor, raster, sf, mapview, ncdf4, gdalUtils,rgdal, ncdf)

# Folder Path -------------------------------------------------------------

local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" }

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" }


#### Load NL data
nl_outpath <- str_glue("{folder_path}/NL_bound.gpkg") 

if (!file.exists(nl_outpath))
{
  print("Loading file")
  nl_bound <- getData("GADM", country = "NL", level = 1) %>% 
    st_as_sf()
  
  st_write(nl_bound, nl_outpath)
} else {
  print("file exists")
  nl_bound <- st_read(nl_outpath)
}

agri_suit <- raster(str_glue("{folder_path}/Data/Data_Global/Cropland/Agri_suitability_mosaic_10.tif"))

###
nl_agri_suit <- str_glue("{folder_path}/Data/Data_NL/Cropland/Agri_suitability_NL_10.tif") 


#### Crop files
# # For LINUX
if (!file.exists(nl_agri_suit))
{
tic("Cropping agriculture suitability")
gdalR::GDAL_crop(agri_suit, nl_agri_suit, shapefile_path = nl_outpath,
                 large_tif = TRUE)
toc()
}

nl_agri_suit <- raster(str_glue("{folder_path}/Data/Data_NL/Cropland/Agri_suitability_NL_10.tif")) 
plot(nl_agri_suit)

# # For 2019
# crop_nl_2019 <- crop(raster(nl_lc_2019_infile),nl_bound)
# nl_lc_2019_outfile <- mask(crop_nl_2019, nl_bound)
# plot(nl_lc_2019_outfile)
# writeRaster(nl_lc_2019_outfile, filename = "NL_landcover_2019",format="GTiff")
# 
# lc_2019 <- raster(nl_lc_2019_infile)
# writeRaster(lc_2019, filename = lc_2019_outfile, format="GTiff")


# out_mask_filename_rcl <- "Projects/Agriculture_modeling/Data/Data_NL/processing_mask_cropped_binary.tif" %>% 
#   milkunize2("archive")