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

if (!file.exists(reclass_agri)){
  q1 <- quantile(agri_suit_resamp, 0.25)
  q2 <- quantile(agri_suit_resamp, 0.5)
  q3 <- quantile(agri_suit_resamp, 0.75)
  
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
countries <- countries[c(42, 43, 106, 165, 166, 183, 233),]


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

for(j in values_reclas_suit){
 
  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  urban_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_urban_raster_{j}.tif")
  
  if (!file.exists(urban_suit)){
    mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={urban_suit} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
  }
  
  urban_suit <- raster(urban_suit)
  print("B")
  
  
  tempfilename2 <- str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif")
  agri_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_agri_raster_{j}.tif")
  
  if (!file.exists(agri_suit)){
    mystring <- str_glue("gdal_calc.py -A {tempfilename2} --outfile={agri_suit} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
  }
  
  agri_suit <- raster(agri_suit)
  
  print("C")
  
  
  # Calculate the overlay between suitability map
  #agri_suit <- crop(extend(agri_suit, urban_suit), urban_suit)
  #all.equal(extent(urban_suit), extent(agri_suit))
  if (!is.null(urban_suit)){
    if (!is.null(agri_suit)){
      urban_and_agri <- overlay(urban_suit,agri_suit,fun=function(x,y){return(x*y)})
    }
    else{
      urban_and_agri <- NULL
    }
  }
  else{
    urban_and_agri <- NULL
  }
  
  print("D")
  

  if (!is.null(urban_and_agri)){
    print("Printing urban_and_agri")
    print(head(urban_and_agri))
    urban_and_agri_area_country <- exactextractr::exact_extract((urban_and_agri * raster::area(urban_and_agri)), country, fun = "sum", max_cells_in_memory = 1e+06)
  }
  else{
    urban_and_agri_area_country <- 0
  }
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_and_agri_area_country_cat_", j))] <- sum(urban_and_agri_area_country)
  print("E")
  
  
  # Calculate the urban area per category in the country
  if (!is.null(urban_suit)){
    print("Printing urban_suit")
    print(head(urban_suit))
    urban_area_in_country <- exactextractr::exact_extract((urban_suit * raster::area(urban_suit)), country, fun = "sum", max_cells_in_memory = 1e+06) 
  }
  else{
    urban_area_in_country <- 0
  }
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_area_in_country_cat_", j))] <- sum(urban_area_in_country)
  
  print("F")
  
  
  # Calculate the agricultural area per category in the country
  if (!is.null(agri_suit)){
    print("Printing agri_suit")
    print(head(agri_suit))
    agri_area_in_country <- exactextractr::exact_extract((agri_suit * raster::area(agri_suit)), country, fun = "sum", max_cells_in_memory = 1e+06) 
  }
  else{
    agri_area_in_country <- 0
  }
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("agri_area_in_country_cat_", j))] <- sum(agri_area_in_country)
}

# urban_suit <- reclas_urban
# urban_suit[urban_suit != 1] <- 1
# urban_suit[is.na(urban_suit)] <- 1

tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
urban_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster3.tif")

mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={urban_suit} --overwrite --calc='1*(A!= 1)+ 1*(A == 1)'")
system(mystring)

urban_suit <- raster(urban_suit)

# Calculate the total area of the country
area_country <- exactextractr::exact_extract((urban_suit * raster::area(urban_suit)), country, fun = "sum", max_cells_in_memory = 1e+06) 
# %>% 
#   cbind(country$CNTRY_NAME)

output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)

# }

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_urbanized_{i}.csv"))
