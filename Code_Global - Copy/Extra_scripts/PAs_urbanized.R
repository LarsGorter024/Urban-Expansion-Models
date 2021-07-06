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


# Country borders ---------------------------------------------------------

# countries <- st_read("V:/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
# countries <- st_read("//milkunstud-srv.science.ru.nl/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
countries <- st_read("/vol/milkundata/World_country_boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
# values_reclas_suit <- unique(reclas_suit)
values_reclas_suit <- 1:4


# Select fewer countries for quicker analysis
# countries <- countries[c(42, 43, 106, 165, 166, 183, 233),]
i <- as.numeric(commandArgs(trailingOnly = TRUE))

# setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses"))

# Country loop (P)PAs ------------------------------------------------------------
if (scale == "NL")
{
  countries <- filter(countries[countries$CNTRY_NAME == "Netherlands", ])
  
}  

# for(i in 1:length(unique(countries$OBJECTID))){

country <- countries[countries$OBJECTID == unique(countries$OBJECTID)[i],]

print(i)

output <- as.data.frame(matrix(data = NA, nrow = length(unique(country$CNTRY_NAME)), ncol = 3+2*length(values_reclas_suit)))
colnames(output) <- c("country", paste0("area_in_pa_cat_", values_reclas_suit), "total_pa_area", paste0("area_in_country_cat_", values_reclas_suit), "total_country_area")
output$country <- unique(country$CNTRY_NAME)

outcsv <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_Global/PAs_urbanized_{i}.csv")

if (!file.exists(outcsv)){
  
  # Load data ---------------------------------------------------------------
  
  reclas_suit <- raster(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif"))
  
  
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
  
  inter_country_PA <- st_intersection(country, PAs_shape)
  
  
  for(j in values_reclas_suit)
  {
    # suit_raster <- reclas_suit
    # suit_raster[suit_raster != j] <- 0
    # suit_raster[suit_raster == j] <- 1
    print(str_glue("j is {j}"))
    tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
    suit_raster_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_{j}.tif")
    suit_raster_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_area_4.tif")
    suit_raster_multiplied_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_multiplied_{j}.tif")
    total_area_filename <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area.tif")
    total_area_filename_multiplied <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/suit_raster_total_area_multiplied.tif")
    
    
    if (!file.exists(suit_raster_filename))
    {
      mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={suit_raster_filename} --overwrite --calc='0*(A!={j})+1*(A=={j})'")
      system(mystring)
    }
    
    suit_raster <- raster(suit_raster_filename) 
    raster::rasterOptions(maxmemory = ncell(suit_raster) - 1)
    
    # Calculate the area of the category in the PAs
    if (!is.null(suit_raster))
    {
      if (!is.null(inter_country_PA))
      {
        print("Printing head suit_raster for in PAs")
        print(head(suit_raster))
        
        if (!file.exists(suit_raster_area_filename))
        {
          tic("Calculating area")
          suit_raster_area <- raster::area(suit_raster, filename = suit_raster_area_filename)
          toc()
          suit_raster_area <- raster(suit_raster_area_filename)
          # 749.178 elapsed
        } else {
          suit_raster_area <- raster(suit_raster_area_filename)
        }
        
        if (!file.exists(suit_raster_multiplied_filename))
        {
          mystring_multiply <- str_glue("gdal_calc.py -A {suit_raster_filename} -B {suit_raster_area_filename} --outfile={suit_raster_multiplied_filename} --overwrite --calc='A*B'")
          system(mystring_multiply)
        }
        
        tic("Multiplying")    
        suit_raster_multiplied <- raster(suit_raster_multiplied_filename)
        toc()
        
        tic("Extracting values")
        area_in_pa <- exactextractr::exact_extract(suit_raster_multiplied, inter_country_PA, fun = "sum") 
        toc()
        area_in_pa[is.infinite(area_in_pa)] <- NA
        output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_pa_cat_", j))] <- sum(area_in_pa, na.rm = TRUE)
      }
      else{
        area_in_pa <- 0
        print(area_in_pa)
        output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_pa_cat_", j))] <- sum(area_in_pa)
      }
      
    }    else{
      area_in_pa <- 0
      print(area_in_pa)
      output[which(output$country == country$CNTRY_NAME), which(colnames(output) == paste0("area_in_pa_cat_", j))] <- sum(area_in_pa)
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
  
  print("After 1st loop")
  tempfilename1 <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/Reclassified_urban_suit_{scale}.tif")
  area_raster <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/temp/area_raster.tif")
  
  if (!file.exists(total_area_filename_multiplied))
  {
    print("GDAL calc area_raster")
    mystring <- str_glue("gdal_calc.py -A {tempfilename1} --outfile={area_raster} --overwrite --calc='1*(A!= 1) + 1*(A == 1)'")
    system(mystring)
    tic("Calculating area")
    total_raster_area <- raster::area(area_raster, filename = total_area_filename)
    toc()
    
    mystring_multiply <- str_glue("gdal_calc.py -A {area_raster} -B {total_area_filename} --outfile={total_area_filename_multiplied} --overwrite --calc='A*B'")
    system(mystring_multiply)
    
    
    tic("Multiplying")    
    total_area_multiplied <- raster(total_area_filename_multiplied)
    toc()
    
    
    # 749.178 elapsed
  } else {
    total_area_multiplied <- raster(total_area_filename_multiplied)
  }
  
  
  
  # Calculate the total area of the PAs
  if (!is.null(inter_country_PA)){
    print("Printing head total_area_multiplied")
    print(head(total_area_multiplied))
    area_pa <- exactextractr::exact_extract(total_area_multiplied, inter_country_PA, fun = "sum") 
    print(area_pa)
    
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_pa_area")] <- sum(area_pa)
  }  else{
    print("Printing head suit_raster")
    area_pa <- 0
    output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_pa_area")] <- sum(area_pa)
    
  }
  
  # Calculate the total area of the country
  area_country <- exactextractr::exact_extract(total_area_multiplied, country, fun = "sum") 
  
  output[which(output$country == country$CNTRY_NAME), which(colnames(output) == "total_country_area")] <- sum(area_country)
  
}
# write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_urbanized_{i}.csv"))
write.csv(output, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_Global/PAs_urbanized_{i}.csv"))


# Relative amount per country in PA total area
# Histogram & Plot of 3 highest ranking countries of relative suit in PA

# account for reprojection; not every cell is the same area, multiply with area with area() function?
