#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=24:00:00
#SBATCH --output "/Logs/absences_eval.log"
#SBATCH --mem=120G

##########################################
####  Create absences
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, foreach, tictoc, sf, GSIF, foreach, doParallel,gdalR)


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


reclassify_gdal <- file.path(str_glue("{folder_path}/Code_Global/GDAL/reclassify_vals_gdal.R"))
source(reclassify_gdal)

rarify_points <- file.path(str_glue("{folder_path}/Code_Global/rarify_points.R"))
source(rarify_points)

# Load data ---------------------------------------------------------------

#############################################
#### Reclassify raster

changes_type = "eval"


if (changes_type == "fit")
{
  mybrick <- stack(str_glue("{datafolder_path}/Data_{scale}/Landcover_maps/landcover_2006_crop.tif"), 
                   str_glue("{datafolder_path}/Data_{scale}/Landcover_maps/landcover_2019_crop.tif"))	
} 
if (changes_type == "eval")
{
  mybrick <- stack(str_glue("{datafolder_path}/Data_{scale}/Landcover_maps/landcover_1993_crop.tif"),
                   str_glue("{datafolder_path}/Data_{scale}/Landcover_maps/landcover_2006_crop.tif"))
}

x <- mybrick[[1]]
y <- mybrick[[2]]

######################################

cores_num <- 20  #40 for full dataset?

# Reclassify landcover ----------------------------------------------------

# "Data/Absences/" %>% 
#   dir.create(recursive = TRUE)

basefolder_path <- str_glue("{datafolder_path}/Data_Global/Absences")

typefolder_path <- str_glue("{basefolder_path}/{changes_type}")
dir.create(typefolder_path, recursive = TRUE)

raster_out_filename <- str_glue("{typefolder_path}/Absences_{changes_type}.tif")
shape_out_filename <- str_glue("{typefolder_path}/Absences_{changes_type}.gpkg")
shape_out_rare_filename <- str_glue("{typefolder_path}/Absences_{changes_type}_rarified.gpkg")

tempfolder_path <- str_glue("{datafolder_path}/Data_Global/Absences/temp")

rasterOptions(maxmemory = ncell(mybrick) - 1)

#### Create absences ####

base_folder <- str_glue("{datafolder_path}/Data_Global/Response_variable/Changes_vector")

exclude_urban = FALSE

########################################


  presences_path <- str_glue("{base_folder}/{changes_type}/Presences/Presence_{tolower(changes_type)}_190_1km.gpkg")
  
  presences_loaded <- st_read(presences_path)
  
  presences_number <- nrow(presences_loaded)
  
  abs_number <- presences_number
  
  #
  #### Get change from two rasters ####
  # Check if raster exists and substract two raster if not
  if (!file.exists(raster_out_filename))
  {
    print("Getting change.")
    
    urban <- 190
    
    values_excluded <- urban
    all_categories <- 1:221
    
    values_excluded <- c(values_excluded, 210:220)
    values_included <- all_categories[-values_excluded]
    
    
    change_raster <- reclassify_vals_gdal(x = x, y = y,
                                          outpath = tempfolder_path, outfile = raster_out_filename,
                                          vals_included = values_included, vals_excluded = values_excluded,
                                          category = 190, number_of_cores = cores_num)
    

    
  } else {
    # Load raster otherwise
    change_raster <- raster(raster_out_filename)
  }
 
  
  #### Get change shapefile ####
  # Check if shapefile exists
  if (!file.exists(shape_out_filename))
  {
    # Function fork - sp argument (decide what changes_type of object does the function returns)

      # Raster to points
      print("Sampling absences.")
    tic("Raster to points")
   
    absences <- raster::sampleRandom(change_raster, (abs_number*1.2), na.rm = TRUE, sp = TRUE)
    toc()
    absences_sf <- st_as_sf(absences)
  
    # Rarify absences ---------------------------------------------------------
    
    bioclim_mask <- raster(str_glue("{datafolder_path}/bioclim_crop.tif"))
    
    if (!file.exists(shape_out_rare_filename))
    {
      cat(paste0("Rarifying category ", "190"), "\n")
      absence_points_1km <- as(absences_sf, "Spatial")
      toc("Rarifying...")
      my_absence_rarified <- rarify_points(absence_points_1km, bioclim_mask)
      st_write(my_absence_rarified[1:abs_number,], shape_out_rare_filename)
      toc()
    } else {
      my_absence_rarified <- st_read(shape_out_rare_filename)
    }
    
    print("Writing Shapefile")
    names(absences) <- "PA"
    st_write(my_absence_rarified[1:abs_number,], shape_out_filename, append=FALSE)


    # } else {
    #   absences <- raster::sampleRandom(both, abs_number * multiplyr, na.rm = TRUE, xy = TRUE, df = TRUE)
    #   absences <- absences[, c("x", "y")]
    #
    #   absences <- sample_n(as.data.frame(absences), abs_number)
    # }
  } else {
    # change_points_sf <- st_read(shape_out_filename)
    print("Done")
  }

  
##############################################################################




