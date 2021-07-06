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


infile <- str_glue("{datafolder_path}/Data_NL/NL_data/NL_landcover_2019.tif") 
outfile <- str_glue("{datafolder_path}/Data_{scale}/Test_crop.tif")

# Output filename
lc_no_water <- str_glue("{datafolder_path}/Data_{scale}/lc_2019_crop_function.tif")
lc_no_water2 <- str_glue("{datafolder_path}/Data_{scale}/lc_2019_crop_function2.tif")

if (!file.exists(lc_no_water2))
{
 
  mystring <- str_glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_NL/NL_data/NL_landcover_2019.tif --outfile={lc_no_water} --calc='0*(A==210)+1*logical_and(A!=210, A>0)'")
  system(mystring)
  
  mystring2 <- str_glue("gdal_translate -of GTiff -a_nodata 0 {lc_no_water} {lc_no_water2}")
  system(mystring2)
  
}


tic("Setting water to NA")
gdal_call <- str_glue("gdal_calc.py -A {lc_no_water2} -B {infile} --outfile={outfile} --calc='B*A'")

system(gdal_call)
toc()

# pre <-  raster(infile)
# plot(pre)
# test <- raster(outfile)
# plot(test)
# freq(test)
# lc_crop <- raster(lc_no_water)
# plot(lc_crop)
# lc_crop2 <- raster(lc_no_water2)
# plot(lc_crop2)


# lc_2019 <- raster("/vol/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.tif")
# lc_2019 <- raster("/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_NL/NL_data/NL_landcover_2019.tif")