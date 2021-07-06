##########################################
####  Quantify how much nature area will be urbanized
##########################################
#### | Project name: Urban modeling
#### | Creator: Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Load packages -----------------------------------------------------------
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")

pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR, exactextractr) #Rahat, pkgmaker, GSIF, foreach, doParallel, doSNOW,

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

# Load data ---------------------------------------------------------------
reclas_urban <- raster(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif"))
# plot(reclas_urban)
# agri_suit_raw <- raster(str_glue("{datafolder_path}/Data_{scale}/Cropland/Agri_suitability_mosaic_10.tif"))
agri_suit_resamp <- str_glue("{datafolder_path}/Data_{scale}/Cropland/Agri_suitability_mosaic_10_resampled.tif")
# agri_suit_raw <- resample(agri_suit_raw, reclas_urban, method = 'bilinear')

if (!file.exists(agri_suit_resamp))
{
  
  tic("Resample agri")
  GDAL_resample2(infile = str_glue("{datafolder_path}/Data_Global/Cropland/Agri_suitability_mosaic_10.tif"), outfile = agri_suit_resamp, target_extent = "-180 -57 180 84",
                 target_resolution = "0.002777777777778", method = "bilinear", large_tif = TRUE)
  toc()
  
}

agri_suit_resamp <- raster(str_glue("{datafolder_path}/Data_{scale}/Cropland/Agri_suitability_mosaic_10_resampled.tif"))

# plot(agri_suit_raw)

# Reclassify suitability to categories ------------------------------------------------
reclass_agri <- str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif")
agri_test <- raster(reclass_agri)
plot(agri_test)

if (!file.exists(reclass_agri)){
  q1 <- quantile(agri_suit_resamp, 0.25)
  q2 <- quantile(agri_suit_resamp, 0.5)
  q3 <- quantile(agri_suit_resamp, 0.75)
  
  raster::rasterOptions(maxmemory = ncell(agri_suit_resamp) - 1)
  
  reclas_agri <- agri_suit_resamp
  reclas_agri[reclas_agri > q3] <-4
  reclas_agri[reclas_agri > q2 & reclas_agri <= q3] <-3
  reclas_agri[reclas_agri > q1 & reclas_agri <= q2] <-2
  reclas_agri[reclas_agri <=q1] <-1
  
  writeRaster(reclas_agri, filename=str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif"),format="GTiff", overwrite=TRUE)
}

values_reclas_suit <- 1:4
i <- as.numeric(commandArgs(trailingOnly = TRUE))

setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses"))

# Country borders ---------------------------------------------------------

# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

# Select fewer countries for quicker analysis
# countries <- countries[c(42, 43, 106, 165, 166, 183, 233),]


# Country loop for displacement -------------------------------------------

if (scale == "NL")
{
  countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])
  
}  
print("A")

# for(i in 1:length(unique(countries$OBJECTID))){

country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]
print(i)

output <- as.data.frame(matrix(data = NA, nrow = length(unique(country$CNTRY_NAME)), ncol = 2+3*length(values_reclas_suit)))
colnames(output) <- c("country", paste0("urban_and_agri_area_country_cat_", values_reclas_suit), paste0("urban_area_in_country_cat_", values_reclas_suit), paste0("agri_area_in_country_cat_", values_reclas_suit), "total_country_area")
output$country <- unique(country$CNTRY_NAME)

# for(j in values_reclas_suit)
# {
j=4

print(str_glue("Running loop j is {j}, i is {i}"))
tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
suit_raster_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_{j}.tif")

tempfilename2 <- str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif")
agri_raster_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_agri_raster_{j}.tif")  

# For Area
suit_raster_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_area_4.tif")
suit_raster_multiplied_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_multiplied_{j}.tif")
agri_raster_multiplied_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/agri_raster_multiplied_{j}.tif")
urban_and_agri_raster_multiplied_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/urban_and_agri_raster_multiplied_{j}.tif")


total_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area.tif")
total_area_filename_multiplied <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area_multiplied.tif")

# URBAN
if (!file.exists(suit_raster_filename))
{
  print("Making suitability raster")
  mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster_filename} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
  system(mystring)
}
print("Trying to load suitability raster")
suit_raster <- raster(suit_raster_filename)
print("Suitability raster loaded") 
raster::rasterOptions(maxmemory = ncell(suit_raster) - 1)

# AGRI
if (!file.exists(agri_raster_filename))
{
  print("Making agri suitability raster")
  mystring <- str_glue("gdal_calc.py -A {tempfilename2} --outfile={agri_raster_filename} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
  system(mystring)
}
print("Trying to load agri suitability raster")
agri_raster <- raster(suit_raster_filename)
print("Suitability raster loaded") 
raster::rasterOptions(maxmemory = ncell(agri_raster) - 1)


urban_and_agri_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/urban_and_agri_filename_{j}.tif")

if (!is.null(suit_raster)){
  if (!is.null(agri_raster)){
    if (!file.exists(urban_and_agri_filename)){
      urban_and_agri <- overlay(suit_raster,agri_raster,fun=function(x,y){return(x*y)})
      writeRaster(urban_and_agri, filename=str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/urban_and_agri_filename_{j}.tif"), format="GTiff", overwrite=TRUE)
    }else{
      urban_and_agri <- raster(urban_and_agri_filename)
    }
  }
  else{
    urban_and_agri <- NULL
  }
} else{
  urban_and_agri <- NULL
}


# Calculate the area of the urban_and_agri category in the country
if (!is.null(urban_and_agri))
{
  print("Printing head suit_raster for in KBAs")
  # print(head(suit_raster))
  
  if (!file.exists(suit_raster_area_filename))
  {
    tic("Calculating area")
    suit_raster_area <- raster::area(suit_raster, filename = suit_raster_area_filename)
    toc()
    # suit_raster_area <- raster(suit_raster_area_filename)
    
    
  } else {
    print("Area raster exists, loading.")
    suit_raster_area <- raster(suit_raster_area_filename)
  }
  print("Area stuff sucessful.")
  
  
  
  if (!file.exists(urban_and_agri_raster_multiplied_filename))
  {
    mystring_multiply <- str_glue("gdal_calc.py -A {urban_and_agri_filename} -B {suit_raster_area_filename} --outfile={urban_and_agri_raster_multiplied_filename} --overwrite --calc='A*B'")
    system(mystring_multiply)
  }
  print("Multiplying stuff sucessful")
  
  tic("Multiplying")    
  urban_and_agri_raster_multiplied <- raster(urban_and_agri_raster_multiplied_filename)
  toc()
  
  tic("Extracting values")
  area_in_country <- exactextractr::exact_extract(urban_and_agri_raster_multiplied, country, fun = "sum") 
  toc()
  area_in_country[is.infinite(area_in_country)] <- NA                  
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_and_agri_area_country_cat_", j))] <- sum(area_in_country, na.rm = TRUE)
} else{
  area_in_country <- 0
  print(area_in_country)
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_and_agri_area_country_cat_", j))] <- sum(area_in_country)
}

## Calculate the area of the urban category in the country
if (!is.null(suit_raster))
{
  if (!file.exists(suit_raster_multiplied_filename))
  {
    mystring_multiply <- str_glue("gdal_calc.py -A {suit_raster_filename} -B {suit_raster_area_filename} --outfile={suit_raster_multiplied_filename} --overwrite --calc='A*B'")
    system(mystring_multiply)
  }
  print("Multiplying stuff sucessful")
  
  tic("Multiplying")    
  suit_raster_multiplied <- raster(suit_raster_multiplied_filename)
  toc()
  
  tic("Extracting values")
  urban_area_in_country <- exactextractr::exact_extract(suit_raster_multiplied, country, fun = "sum") 
  toc()
  urban_area_in_country[is.infinite(urban_area_in_country)] <- NA                  
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_area_in_country_cat_", j))] <- sum(urban_area_in_country, na.rm = TRUE)
} else{
  urban_area_in_country <- 0
  print(urban_area_in_country)
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_area_in_country_cat_", j))] <- sum(urban_area_in_country)
}


### Calculate the area of the agri category in the country
if (!is.null(agri_raster))
{
  if (!file.exists(agri_raster_multiplied_filename))
  {
    mystring_multiply <- str_glue("gdal_calc.py -A {agri_raster_filename} -B {suit_raster_area_filename} --outfile={agri_raster_multiplied_filename} --overwrite --calc='A*B'")
    system(mystring_multiply)
  }
  print("Multiplying stuff sucessful")
  
  tic("Multiplying")    
  agri_raster_multiplied <- raster(agri_raster_multiplied_filename)
  toc()
  
  tic("Extracting values")
  agri_area_in_country <- exactextractr::exact_extract(agri_raster_multiplied, country, fun = "sum") 
  toc()
  agri_area_in_country[is.infinite(agri_area_in_country)] <- NA                  
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("agri_area_in_country_cat_", j))] <- sum(agri_area_in_country, na.rm = TRUE)
} else{
  agri_area_in_country <- 0
  print(agri_area_in_country)
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("agri_area_in_country_cat_", j))] <- sum(agri_area_in_country)
}

# }

# Calculate the total area of the country

tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
area_raster_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/area_raster.tif")

if (!file.exists(total_area_filename_multiplied))
{
  print("GDAL calc area_raster")
  mystring_tot <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={area_raster_filename} --overwrite --calc='1*(A!= 1) + 1*(A == 1)'")
  if (!file.exists(area_raster_filename))
  {
    system(mystring_tot)  
  }
  tic("Calculating area")
  area_raster <- raster(area_raster_filename)
  total_raster_area <- raster::area(area_raster, filename = total_area_filename)
  toc()
  
  mystring_multiply <- str_glue("gdal_calc.py -A {area_raster_filename} -B {total_area_filename} --outfile={total_area_filename_multiplied} --overwrite --calc='A*B'")
  system(mystring_multiply)
  
  
  tic("Multiplying")    
  total_area_multiplied <- raster(total_area_filename_multiplied)
  toc()
  
  
  # 749.178 elapsed
} else {
  total_area_multiplied <- raster(total_area_filename_multiplied)
}


area_country <- exactextractr::exact_extract(total_area_multiplied, country, fun = "sum") 

output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)


# }

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_Global/Cropland_urbanized_{i}.csv"))

# test_displ <- raster(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/urban_and_agri_filename_4.tif"))
# plot(test_displ)

