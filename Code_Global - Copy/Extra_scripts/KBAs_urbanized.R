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
pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR, glue, exactextractr)

# Folder Path -------------------------------------------------------------
scale <- "Global"
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
urban_suit <- raster(str_glue("{datafolder_path}/Data_{scale}/Model_output/Prediction_merged_eval_190_new.tif"))
# plot(urban_suit)

# KBAs <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_final/KBAs_fnl.tif"))
KBAs_shape <- st_read(str_glue("{datafolder_path}/Predictors/KBAs/KBAsGlobal_2020_September_02_POL.shp"))
KBAs_shape <- KBAs_shape[(is.na(KBAs_shape$KbaStatus) | KBAs_shape$KbaStatus == "confirmed" | KBAs_shape$KbaStatus == "candidate" | KBAs_shape$KbaStatus == "proposed"), ]
#class(KBAs_shape)

reclas_suit <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")

# Reclassify suitability to categories ------------------------------------------------
if (!file.exists(reclas_suit))
{
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
  
  writeRaster(reclas_suit, filename=str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif"),format="GTiff", overwrite=TRUE)
} else{
  reclas_suit <- raster(reclas_suit)
}

# Country borders ---------------------------------------------------------

# countries <- st_read("V:/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")

### Select fewer countries for quicker analysis
# countries <- countries[c(42, 43, 106, 165, 166, 183, 233),]


values_reclas_suit <- 1:4

i <- as.numeric(commandArgs(trailingOnly = TRUE))

#setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses"))

# Country loop KBA ------------------------------------------------------------
if (scale == "NL")
{
  countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])
  
}  

# for(i in 1:length(unique(countries$OBJECTID))){

country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]
inter_country_KBA <- st_intersection(country, KBAs_shape)
#print(i)

output <- as.data.frame(matrix(data = NA, nrow = length(unique(country$CNTRY_NAME)), ncol = 3+2*length(values_reclas_suit)))
colnames(output) <- c("country", paste0("area_in_kba_cat_", values_reclas_suit), "total_kba_area", paste0("area_in_country_cat_", values_reclas_suit), "total_country_area")
output$country <- unique(country$CNTRY_NAME)


for(j in values_reclas_suit)
{
  print(str_glue("Running loop j is {j}, i is {i}"))
  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  suit_raster_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_{j}.tif")
  suit_raster_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_area_4.tif")
  suit_raster_multiplied_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_multiplied_{j}.tif")
  total_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area.tif")
  total_area_filename_multiplied <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area_multiplied.tif")
  
  
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
  
  # Calculate the area of the category in the KBAs
  if (!is.null(suit_raster))
  {
    if (!is.null(inter_country_KBA))
    {
      print("Printing head suit_raster for in KBAs")
      # print(head(suit_raster))
      
      if (!file.exists(suit_raster_area_filename))
      {
        tic("Calculating area")
        suit_raster_area <- raster::area(suit_raster, filename = suit_raster_area_filename)
        toc()
        # suit_raster_area <- raster(suit_raster_area_filename)
        
        # 749.178 elapsed
      } else {
        print("Area raster exists, loading.")
        suit_raster_area <- raster(suit_raster_area_filename)
      }
      print("Area stuff sucessful.")
      
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
      area_in_kba <- exactextractr::exact_extract(suit_raster_multiplied, inter_country_KBA, fun = "sum") 
      toc()
      area_in_kba[is.infinite(area_in_kba)] <- NA                  
      
      output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_kba_cat_", j))] <- sum(area_in_kba, na.rm = TRUE)
    }
    else{
      area_in_kba <- 0
      print(area_in_kba)
      output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_kba_cat_", j))] <- sum(area_in_kba)
    }
    
  }    else{
    area_in_kba <- 0
    print(area_in_kba)
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_kba_cat_", j))] <- sum(area_in_kba)
  }
  
  # Calculate the area of the category in the country
  print("Printing head suit_raster for in country")
  print(head(suit_raster))
  if (!is.null(suit_raster)){
    
    area_in_country <- exactextractr::exact_extract(suit_raster_multiplied, country, fun = "sum") 
    area_in_country[is.infinite(area_in_country)] <- NA 
  }    else{
    area_in_country <- 0
    
  }
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_country_cat_", j))] <- sum(area_in_country, na.rm = TRUE)
  
}

# suit_raster2 <- reclas_suit
# suit_raster2[suit_raster2 != 1] <- 1
# suit_raster2[is.na(suit_raster2)] <- 1

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



# Calculate the total area of the KBAs
if (!is.null(inter_country_KBA)){
  print("Printing head total_area_multiplied")
  print(head(total_area_multiplied))
  area_kba <- exactextractr::exact_extract(total_area_multiplied, inter_country_KBA, fun = "sum") 
  print(area_kba)
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_kba_area")] <- sum(area_kba)
}  else{
  print("Printing head suit_raster")
  area_kba <- 0
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_kba_area")] <- sum(area_kba)
  
}
# Calculate the total area of the country
area_country <- exactextractr::exact_extract(total_area_multiplied, country, fun = "sum") 
# cbind(country$CNTRY_NAME)

output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)

# }

write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/KBAs_Global/KBAs_urbanized_{i}.csv"))

# Relative amount per country in KBA total area, in csv F/G
# Histogram & Plot of 3 highest ranking countries of relative suit in KBA


