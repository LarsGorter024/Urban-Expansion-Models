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
# install.packages("exactextractr")
pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR, glue, exactextractr)

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
# Remove 1 -  after inversed output is fixed
urban_suit <- 1 - raster(str_glue("{datafolder_path}/Data_{scale}/Model_output/Prediction_merged_eval_190_new_april.tif"))
# plot(urban_suit)

KBAs <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/KBAs_fnl.tif"))
KBAs_shape <- st_read(str_glue("{datafolder_path}/Predictors/KBAs/KBAsGlobal_2020_September_02_POL.shp"))
KBAs_shape <- KBAs_shape[(is.na(KBAs_shape$KbaStatus) | KBAs_shape$KbaStatus == "confirmed" | KBAs_shape$KbaStatus == "candidate" | KBAs_shape$KbaStatus == "proposed"), ]
class(KBAs_shape)

reclas_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")

# Reclassify suitability to categories ------------------------------------------------
q1 <- quantile(urban_suit, 0.25)
q2 <- quantile(urban_suit, 0.5)
q3 <- quantile(urban_suit, 0.75)

reclas_suit <- urban_suit
reclas_suit[reclas_suit > q3] <-4
reclas_suit[reclas_suit > q2 & reclas_suit <= q3] <-3
reclas_suit[reclas_suit > q1 & reclas_suit <= q2] <-2
reclas_suit[reclas_suit <=q1] <-1

plot(reclas_suit)

raster(reclas_suit)

# writeraster reclass for displacement analysis
writeRaster(reclas_suit, filename=str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif"),format="GTiff", overwrite=TRUE)


# Country borders ---------------------------------------------------------

# countries <- st_read("V:/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

# NL_crop <- countries[countries$CNTRY_NAME == "Netherlands", ]

output <- as.data.frame(matrix(data = NA, nrow = length(unique(countries$CNTRY_NAME)), ncol = 3+2*length(unique(reclas_suit))))
colnames(output) <- c("country", paste0("area_in_kba_cat_", unique(reclas_suit)), "total_kba_area", paste0("area_in_country_cat_", unique(reclas_suit)), "total_country_area")
output$country <- unique(countries$CNTRY_NAME)


# Country loop KBA ------------------------------------------------------------
if (scale == "NL")
{
countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])

}  
  
for(i in 1:length(unique(countries$OBJECTID))){
  
  country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]
  inter_country_KBA <- st_intersection(country, KBAs_shape)
  
  for(j in unique(reclas_suit)){
    # suit_raster <- reclas_suit
    # suit_raster[suit_raster != j] <- 0
    # suit_raster[suit_raster == j] <- 1
    
    tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
    suit_raster <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster.tif")
    
    mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
    system(mystring)
    suit_raster <- raster(suit_raster)
    
    # Calculate the area of the category in the KBAs
    print("Calculate the area of the category in the KBAs")
    tic()
    area_in_kba <- exactextractr::exact_extract((suit_raster * raster::area(suit_raster)), inter_country_KBA, fun = "sum") 
    # %>% 
    #   cbind(inter_country_KBA$NatName)
    toc()
    print(class(area_in_kba))
    print(length(area_in_kba))
    print(nrow(inter_country_KBA))
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_kba_cat_", j))] <- sum(area_in_kba)
    
    # Calculate the area of the category in the country
    area_in_country <- exactextractr::exact_extract((suit_raster * raster::area(suit_raster)), country, fun = "sum") 
    # %>% 
    #   cbind(country$CNTRY_NAME)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_country_cat_", j))] <- sum(area_in_country)
    
  }
  
  # suit_raster2 <- reclas_suit
  # suit_raster2[suit_raster2 != 1] <- 1
  # suit_raster2[is.na(suit_raster2)] <- 1
  
  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  suit_raster <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster.tif")
  
  mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster} --overwrite --calc='1*(A!= 1) + 1*(A == 1)'")
  system(mystring)
  
  suit_raster <- raster(suit_raster)

  # Calculate the total area of the KBAs
  area_kba <- exactextractr::exact_extract((suit_raster * raster::area(suit_raster)), inter_country_KBA, fun = "sum") 
  # %>% 
  #   cbind(inter_country_KBA$NatName)
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_kba_area")] <- sum(area_kba)
  
  # Calculate the total area of the country
  area_country <- exactextractr::exact_extract((suit_raster * raster::area(suit_raster)), country, fun = "sum") 
  # %>% 
  #   cbind(country$CNTRY_NAME)
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)
  
}

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/KBAs_urbanized.csv"))

# Relative amount per country in KBA total area, in csv F/G
# Histogram & Plot of 3 highest ranking countries of relative suit in KBA


