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

pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR) #Rahat, pkgmaker, GSIF, foreach, doParallel, doSNOW,

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


# Load data ---------------------------------------------------------------
reclas_urban <- raster(str_glue("{datafolder_path}/Data_{scale}/Reclassified_urban_suit_{scale}.tif"))
# plot(reclas_urban)
agri_suit_raw <- raster(str_glue("{datafolder_path}/Data_{scale}/Cropland/Agri_suitability_NL_10.tif"))
agri_suit_raw <- resample(agri_suit_raw, reclas_urban,method = 'bilinear')
# plot(agri_suit_raw)

# Reclassify suitability to categories ------------------------------------------------
q1 <- quantile(agri_suit_raw, 0.25)
q2 <- quantile(agri_suit_raw, 0.5)
q3 <- quantile(agri_suit_raw, 0.75)

reclas_agri <- agri_suit_raw
reclas_agri[reclas_agri > q3] <-4
reclas_agri[reclas_agri > q2 & reclas_agri <= q3] <-3
reclas_agri[reclas_agri > q1 & reclas_agri <= q2] <-2
reclas_agri[reclas_agri <=q1] <-1

plot(reclas_agri)
summary(reclas_agri)
freq(reclas_agri)
# raster(reclas_agri)

writeRaster(reclas_agri, filename=str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif"),format="GTiff", overwrite=TRUE)

# Country borders ---------------------------------------------------------

# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

output <- as.data.frame(matrix(data = NA, nrow = length(unique(countries$CNTRY_NAME)), ncol = 2+3*length(unique(reclas_urban))))
colnames(output) <- c("country", paste0("urban_and_agri_area_country_cat_", unique(reclas_urban)), paste0("urban_area_in_country_cat_", unique(reclas_urban)), paste0("agri_area_in_country_cat_", unique(reclas_urban)), "total_country_area")
output$country <- unique(countries$CNTRY_NAME)


# Country loop for displacement -------------------------------------------

if (scale == "NL")
{
  countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])
  
}  

# agri_suit[urban_suit != 1] <- NA
# 
# agri_suit[agri_suit == 4]<- 1
# agri_suit <- agri_suit * area(agri_suit)
# sum(agri_suit[is.na(urban_suit)], na.rm = T)


for(i in 1:length(unique(countries$OBJECTID))){
  
  country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]
 
  for(j in unique(reclas_urban)){
    # urban_suit <- reclas_urban
    # urban_suit[urban_suit != j] <- 0
    # urban_suit[urban_suit == j] <- 1
    tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
    urban_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_urban_raster.tif")
    
    mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={urban_suit} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
    
    urban_suit <- raster(urban_suit)
    
    
    # Does it work like this?
    # agri_suit <- reclas_agri
    # agri_suit[agri_suit != j] <- 0
    # agri_suit[agri_suit == j] <- 1
    tempfilename2 <- str_glue("{datafolder_path}/Data_{scale}/Reclassified_agri_suit_{scale}.tif")
    agri_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_agri_raster.tif")
    
    mystring <- str_glue("gdal_calc.py -A {tempfilename2} --outfile={agri_suit} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
    
    agri_suit <- raster(agri_suit)
    
    # Calculate the overlay between suitability map
    #agri_suit <- crop(extend(agri_suit, urban_suit), urban_suit)
    #all.equal(extent(urban_suit), extent(agri_suit))
    
    urban_and_agri <- overlay(urban_suit,agri_suit,fun=function(x,y){return(x*y)})
  
    urban_and_agri_area_country <- raster::extract((urban_and_agri * raster::area(urban_and_agri)), country, fun = sum, na.rm = T, df = T) %>% 
      cbind(country$CNTRY_NAME)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_and_agri_area_country_cat_", j))] <- sum(urban_and_agri_area_country$layer)
    
    # Calculate the urban area per category in the country
    urban_area_in_country <- raster::extract((urban_suit * raster::area(urban_suit)), country, fun = sum, na.rm = T, df = T) %>% 
      cbind(country$CNTRY_NAME)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("urban_area_in_country_cat_", j))] <- sum(urban_area_in_country$layer)
    
    # Calculate the agricultural area per category in the country
    agri_area_in_country <- raster::extract((agri_suit * raster::area(agri_suit)), country, fun = sum, na.rm = T, df = T) %>% 
      cbind(country$CNTRY_NAME)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("agri_area_in_country_cat_", j))] <- sum(agri_area_in_country$layer)
  }
  
  # urban_suit <- reclas_urban
  # urban_suit[urban_suit != 1] <- 1
  # urban_suit[is.na(urban_suit)] <- 1

  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  urban_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster.tif")
  
  mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={urban_suit} --overwrite --calc='1*(A!= 1)+ 1*(A == 1)'")
  system(mystring)
  
  urban_suit <- raster(urban_suit)
  
  # Calculate the total area of the country
  area_country <- raster::extract((urban_suit * raster::area(urban_suit)), country, fun = sum, na.rm = T, df = T) %>% 
    cbind(country$CNTRY_NAME)
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country$layer)
  
}

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_urbanized.csv"))
