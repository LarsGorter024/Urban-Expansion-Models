##########################################
####  Population density
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################


# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, glue, RStoolbox, sf, gdalR, tictoc)


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

# Load GPW population density file
print("Loading files")
pop_1990 <- str_glue("{datafolder_path}/Predictors/GWP/gl_gpwv3_pdens_90_wrk_25/gldens90/glds90ag/w001001.adf")
pop_2005 <-str_glue("{datafolder_path}/Predictors/GWP/gpw-v4-population-density-adjusted-2005_2,5arcmin/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif")

pop_1990_raw <- pop_1990 %>% 
  raster()
pop_2005_raw <- pop_2005 %>% 
  raster()

# logtransform the population data -------------------------------------
in_grid_1990 <- pop_1990_raw
in_grid_2005 <- pop_2005_raw
out_grid_1990 <- str_glue("{datafolder_path}/Data_{scale}/Predictors_temp/Pop_density_1990_logtr.tif")
out_grid_2005 <- str_glue("{datafolder_path}/Data_{scale}/Predictors_temp/Pop_density_2005_logtr.tif")

print("Log transformations")
if (!file.exists(out_grid_1990))
{
  print(in_grid_1990)
  print(getwd())
  logtr_command <- glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/GWP/gl_gpwv3_pdens_90_wrk_25/gldens90/glds90ag/w001001.adf --calc \"log10(A+1)\" --type Float32 --outfile {out_grid_1990}")
  system(logtr_command)
  
}

if (!file.exists(out_grid_2005))
{
  logtr_command <- glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Predictors/GWP/gpw-v4-population-density-adjusted-2005_2,5arcmin/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif --calc \"log10(A+1)\" --type Float32 --outfile {out_grid_2005}")
  system(logtr_command)
  
}

print("load log variables")
# # Load the logtransformed variable
pop_1990_logtr <- out_grid_1990 %>%
  raster()

pop_2005_logtr <- out_grid_2005 %>%
  raster()

# Normalize
print("Normalizing")
pop_1990_norm <- normImage(pop_1990_logtr, norm = TRUE)
pop_2005_norm <- normImage(pop_2005_logtr, norm = TRUE)

# pop_1990_norm[is.na(pop_1990_norm)] <- 0 
# pop_2005_norm[is.na(pop_2005_norm)] <- 0

popnorm_1990_outname <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_1990_normalized.tif")
popnorm_2005_outname <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_2005_normalized.tif")

if (!file.exists(popnorm_1990_outname))
{
  print("writing raster")
  writeRaster(pop_1990_norm, popnorm_1990_outname, options = "COMPRESS=LZW")
}

if (!file.exists(popnorm_2005_outname))
{
  writeRaster(pop_2005_norm, popnorm_2005_outname, options = "COMPRESS=LZW")
}


# Resample ----------------------------------------------------------------
outname_1990_resamp <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_1990_resamp.tif")
outname_2005_resamp <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_2005_resamp.tif")

pop_1990_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_1990_fnl.tif")
pop_2005_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_2005_fnl.tif")

if (!file.exists(outname_1990_resamp))
{
  # Resample layer, outfile was popnorm_outname
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_1990_normalized.tif"), outfile = outname_1990_resamp, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
}

if (!file.exists(outname_2005_resamp))
{
  # Resample layer, outfile was popnorm_outname
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_2005_normalized.tif"), outfile = outname_2005_resamp, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
}



pop_1990_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_1990_resamp.tif"), 
                              outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_1990_fnl.tif"), 
                              scale = scale)

pop_2005_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/Pop_density_2005_resamp.tif"), 
                              outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_2005_fnl.tif"), 
                              scale = scale)

test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_1990_fnl.tif"))
plot(test, zlim= c(-3,5))

test <-  raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/Pop_density_2005_fnl.tif"))
plot(test)
