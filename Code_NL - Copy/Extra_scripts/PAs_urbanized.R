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
pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR, glue)

# Folder Path -------------------------------------------------------------
scale <- "NL"
local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{ # folder_path <- "R:/ES_students/lgorter/Urban_Expansion_model"
  #datafolder_path <- "X:/ES_students/lgorter/Urban_Expansion_model/Data" 
  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" 
  datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data" 
}

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"
}



# Load data ---------------------------------------------------------------

reclas_suit <- raster(str_glue("{datafolder_path}/Data_{scale}/Reclassified_urban_suit_{scale}.tif"))


p_areas1 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp0/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
  st_read()

p_areas2 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp1/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
  st_read()

p_areas3 <- str_glue("{datafolder_path}/Predictors/WDPA/WDPA_WDOECM_wdpa_shp2/WDPA_WDOECM_wdpa_shp-polygons.shp") %>%
  st_read()

PAs <- rbind(p_areas1, p_areas2, p_areas3)

class(p_areas1)
class(PAs)

PAs_shape <- PAs %>%
  filter(MARINE == 0)  %>%
  filter(STATUS != "Proposed")

class(PAs_shape)

PPAs_shape <- PAs %>%
  filter(MARINE == 0)  %>%
  filter(STATUS == "Proposed")


###

# Country borders ---------------------------------------------------------

# countries <- st_read("V:/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
class(countries)

output <- as.data.frame(matrix(data = NA, nrow = length(unique(countries$CNTRY_NAME)), ncol = 3+2*length(unique(reclas_suit))))
# output_ppa <- as.data.frame(matrix(data = NA, nrow = length(unique(countries$CNTRY_NAME)), ncol = 3+2*length(unique(reclas_suit))))

colnames(output) <- c("country", paste0("area_in_pa_cat_", unique(reclas_suit)), "total_pa_area", paste0("area_in_country_cat_", unique(reclas_suit)), "total_country_area")
# colnames(output_ppa) <- c("country", paste0("area_in_ppa_cat_", unique(reclas_suit)), "total_ppa_area", paste0("area_in_country_cat_", unique(reclas_suit)), "total_country_area")

output$country <- unique(countries$CNTRY_NAME)


# Country loop (P)PAs ------------------------------------------------------------
if (scale == "NL")
{
  countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])
  
}  

for(i in 1:length(unique(countries$OBJECTID))){
  
  country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]
  inter_country_PA <- st_intersection(country, PAs_shape)
  # inter_country_PPA <- st_intersection(country, PPAs_shape)
  print(i)
  
  for(j in unique(reclas_suit)){
    # suit_raster2 <- reclas_suit
    # suit_raster2[suit_raster2 != j] <- 0
    # suit_raster2[suit_raster2 == j] <- 1
    print(str_glue("j is {j}"))
    
    tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
    suit_raster2 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster2.tif")
    
    mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster2} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
    
    suit_raster2 <- raster(suit_raster2)
    
    # Calculate the area of the category in the PAs
    if (!is.null(inter_country_PA)){
      area_in_pa <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), inter_country_PA, fun = "sum") 
      print(area_in_pa)
      output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_pa_cat_", j))] <- sum(area_in_pa)
    }
    else{
      area_in_pa <- 0
      print(area_in_pa)
      output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_pa_cat_", j))] <- sum(area_in_pa)
      
    }
    # if (!is.null(inter_country_PPA)){
    #   area_in_ppa <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), inter_country_PPA, fun = "sum") 
    #   print(area_in_ppa)
    #   output_ppa[which(output_ppa$country == country$CNTRY_NAME), which(colnames(output_ppa) == paste0("area_in_ppa_cat_", j))] <- sum(area_in_ppa)
    # }
    # else{
    #   area_in_ppa <- 0
    #   print(area_in_ppa)
    #   output_ppa[which(output_ppa$country == country$CNTRY_NAME), which(colnames(output_ppa) == paste0("area_in_ppa_cat_", j))] <- sum(area_in_ppa)
    #   
    # }
    # Calculate the area of the category in the country
    print("Second EE")
    area_in_country <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), country, fun = "sum") 
    # %>% 
    #   cbind(country$CNTRY_NAME)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_country_cat_", j))] <- sum(area_in_country)
    # output_ppa[which(output_ppa$country == country$CNTRY_NAME), which(colnames(output_ppa) == paste0("area_in_country_cat_", j))] <- sum(area_in_country)
    
  }
  print("After 1st loop")
  
  # suit_raster2 <- reclas_suit
  # suit_raster2[suit_raster2 != 1] <- 1
  # suit_raster2[is.na(suit_raster2)] <- 1
  
  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  suit_raster2 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster2.tif")
  
  mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster2} --overwrite --calc='1*(A!= 1) + 1*(A == 1)'")
  system(mystring)
  
  suit_raster2 <- raster(suit_raster2)
  
  # Calculate the total area of the PAs
  if (!is.null(inter_country_PA)){
    area_pa <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), inter_country_PA, fun = "sum") 
    print(area_pa)
  }
  else{
    area_pa <- 0
    print(area_pa)
  }
  
  # if (!is.null(inter_country_PPA)){
  #   area_ppa <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), inter_country_PPA, fun = "sum") 
  #   print(area_ppa)
  #   }
  # else{
  #   area_ppa <- 0
  #   print(area_ppa)
  # }
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_pa_area")] <- sum(area_pa)
  # output_ppa[which(output_ppa$country == country$CNTRY_NAME), which(colnames(output_ppa) == "total_ppa_area")] <- sum(area_ppa)
  
  # Calculate the total area of the country
  area_country <- exactextractr::exact_extract((suit_raster2 * raster::area(suit_raster2)), country, fun = "sum") 
  # %>% 
  #   cbind(country$CNTRY_NAME)
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)
  # output_ppa[which(output_ppa$country == country$CNTRY_NAME), which(colnames(output_ppa) == "total_country_area")] <- sum(area_country)
  
}

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_urbanized.csv"))