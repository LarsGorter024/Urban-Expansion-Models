##########################################
####  Population density
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################


# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, glue, RStoolbox,sf, gdalR, tictoc)


# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
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

# Load GPW population density file
print("Loading files")
pop_1990 <- str_glue("{datafolder_path}/Predictors/GWP/gl_gpwv3_pdens_90_wrk_25/gldens90/glds90ag/w001001.adf")
pop_2005 <-str_glue("{datafolder_path}/Predictors/GWP/gpw-v4-population-density-adjusted-2005_2,5arcmin/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif")

pop_1990_raw <- pop_1990 %>% 
  raster()
pop_2005_raw <- pop_2005 %>% 
  raster()

# logtransform the population data -------------------------------------
# Set input and output filenames
in_grid_1990 <- pop_1990_raw
in_grid_2005 <- pop_2005_raw
out_grid_1990 <- str_glue("{datafolder_path}/Data_NL/Predictors_temp/Pop_density_1990_logtr.tif")
out_grid_2005 <- str_glue("{datafolder_path}/Data_NL/Predictors_temp/Pop_density_2005_logtr.tif")

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

popnorm_1990_outname <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_1990_normalized.tif")
popnorm_2005_outname <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_2005_normalized.tif")


if (!file.exists(popnorm_1990_outname))
{
  print("writing raster")
  writeRaster(pop_1990_norm, popnorm_1990_outname, options = "COMPRESS=LZW")
}

if (!file.exists(popnorm_2005_outname))
{
  writeRaster(pop_2005_norm, popnorm_2005_outname, options = "COMPRESS=LZW")
}


# NL crop -----------------------------------------------------------------
nl_outpath <- str_glue("{folder_path}/NL_bound.gpkg") 
nl_bound <- st_read(nl_outpath)

nl_popdens_1990 <- crop(raster(popnorm_1990_outname),nl_bound)
nl_popdens_2005 <- crop(raster(popnorm_2005_outname),nl_bound)

# nl_popdens_1990 <- mask(nl_popdens_1990, nl_bound)
# nl_popdens_2005 <- mask(nl_popdens_2005, nl_bound)

# nl_popdens_1990[is.na(nl_popdens_1990)] <- 0 
# nl_popdens_2005[is.na(nl_popdens_2005)] <- 0



writeRaster(nl_popdens_1990, filename = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_1990_NL.tif"),format="GTiff",overwrite=TRUE)
writeRaster(nl_popdens_2005, filename = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_2005_NL.tif"),format="GTiff",overwrite=TRUE)

pop_1990_pop <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_1990_NL.tif"))
pop_2005_pop <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_2005_NL.tif"))


outname_1990_resamp <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_1990_resamp.tif")
outname_2005_resamp <- str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_2005_resamp.tif")

pop_1990_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_1990_fnl.tif")
pop_2005_fnl <- str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_2005_fnl.tif")


# Resample ----------------------------------------------------------------
 if (!file.exists(outname_1990_resamp))
 {
   GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Normalized/popdens_1990_NL_norm.tif"), outfile = outname_1990_resamp, target_extent = "3.360782 50.723492  7.227095 53.554584",
                  target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
 }


 if (!file.exists(outname_2005_resamp))
 {
   GDAL_resample2(infile = str_glue("{datafolder_path}/Predictors/Normalized/popdens_2005_NL_norm.tif"), outfile = outname_2005_resamp, target_extent = "3.360782 50.723492  7.227095 53.554584",
                  target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
 }

pop_1990_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_1990_resamp.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_1990_fnl.tif"), 
                         scale = "NL")

pop_2005_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_NL/Predictors_intermediate/Pop_density_2005_resamp.tif"), 
                         outfile = str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_2005_fnl.tif"), 
                         scale = "NL")

# test_1990 <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_1990_fnl.tif"))
# plot(test_1990)
# plot(!is.na(test_1990))
 
# test_2005 <- raster(str_glue("{datafolder_path}/Data_NL/Predictors_final/Pop_density_2005_fnl.tif"))
# plot(test_2005)
# plot(!is.na(test_2005))

#