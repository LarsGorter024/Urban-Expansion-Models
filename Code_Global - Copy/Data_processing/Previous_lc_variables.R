#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=4:00:00
#SBATCH --output "/Logs/Previous_lc_calc_FE.log"
#SBATCH --mem=32G

##########################################
####  Previous landcover
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic & Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc, glue, janitor, rgdal, sf)

scale <- "Global"


folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

gdal_resample <- file.path(str_glue("{folder_path}/Code_{scale}/GDAL/gdal_resample.R"))
source(gdal_resample)

Crop_function <- file.path(str_glue("{folder_path}/Code_{scale}/Data_processing/Crop_function.R"))
source(Crop_function)

# Set filenames -----------------------------------------------------------
for (type in c("eval", "fit"))
{
  
  if (type == "fit")
  {
    
    esa_path <- "/vol/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2005-v2.0.7.tif"
    
  }
  
  if (type == "eval")
  {
    
    esa_path <- "/vol/milkundata/ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7.tif"
    
  }
  
  
   # Define conversion strings -----------------------------------------------
  
  crops <- "1*(A>42)+2*logical_and(A>1, A<=42)"
  urban <- "1*(A<=189)+2*logical_and(A>189,A<=191)+1*(A>191)"
  forest <- "1*(A<=48)+2*logical_and(A>49,A<=101)+1*(A>101)"
  grassland <- "1*(A<=108)+2*logical_and(A>109,A<=154)+1*(A>155)"
  wetland <- "1*(A<=158)+2*logical_and(A>159,A<=181)+1*(A>182)"
  
  # Define function
  reclassify_crops <- function(input, category, type, string, del = FALSE)
  {
    outname <- str_glue("{datafolder_path}/Data_{scale}/Predictors_temp/ESA_{category}_{type}_reclassified.tif")
    outname2 <- str_glue("{datafolder_path}/Data_{scale}/Predictors_temp/ESA_{category}_{type}_binary.tif") 
    
    if (isTRUE(del))
    {
      file.remove(outname2)
      file.remove(outname)
    }
    print(outname)
    
    tic("Running")
    if (!file.exists(outname))
    {
      mystring <- glue::glue("gdal_calc.py -A {input} --outfile={outname} --calc=\"{string}\" --NoDataValue=0")
      system(mystring)
      # outname[is.na(outname)] <- 0
    }
    
    
    if (!file.exists(outname2))
    {
      mystring2 <- glue::glue("gdal_calc.py -A {outname} --outfile={outname2} --calc=\"A-1\" --NoDataValue=-1")
      system(mystring2) 
    }
    
    
    toc()
    
  }
  
  # Run functions and reclassify
  reclassify_crops(esa_path, "crops", type = type, crops, del = F)
  reclassify_crops(esa_path, "forest", type = type, forest, del = F)
  reclassify_crops(esa_path, "grassland", type = type, grassland, del = F)
  reclassify_crops(esa_path, "wetland", type = type, wetland, del = F)
  reclassify_crops(esa_path, "urban", type = type, urban, del = F)
  
}
# Harmonize ---------------------------------------------------------------

files_list <- str_glue("{datafolder_path}/Data_{scale}/Predictors_temp") %>%
  list.files(pattern = str_glue("ESA.*._binary"), full.names = TRUE)

for (type in c("eval", "fit"))
{
  for (i in seq_along(files_list))
  {
    #   
    outfile_string <- files_list[i] %>%
      str_remove(str_glue("{datafolder_path}/Data_{scale}/Predictors_temp")) %>%
      str_remove("_binary.tif")
    #   
    layer_resample_name <- str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/{outfile_string}_resamp.tif")
    layer_name_fnl <- str_glue("{datafolder_path}/Data_{scale}/Predictors_final/{outfile_string}_fnl.tif")
    #     
    if (!file.exists(layer_resample_name))
    {
      
      tic("Resampling...")
      GDAL_resample2(infile = files_list[i], outfile = layer_resample_name, target_extent = "-180 -57 180 84",
                     target_resolution = "0.002777777777778", method = "near", large_tif = TRUE)
      toc()
      
    }  
    
    layer_fnl <- Crop_function(infile = str_glue("{datafolder_path}/Data_{scale}/Predictors_intermediate/{outfile_string}_resamp.tif"), 
                               outfile = str_glue("{datafolder_path}/Data_{scale}/Predictors_final/{outfile_string}_fnl.tif"), 
                               scale = scale)
    
  }
}

forest <- raster(str_glue("{datafolder_path}/Data_{scale}/Predictors_temp/ESA_forest_fit_reclassified.tif"))
plot(forest)
print(freq(forest))

# 