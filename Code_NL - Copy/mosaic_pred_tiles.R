##########################################
####  Combine predicted tiles into a mosaic
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Load packages -----------------------------------------------------------
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
# folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model"
# datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"


GDAL_mosaic_tile <- file.path(str_glue("{folder_path}/Code_NL/GDAL/GDAL_mosaic_tile.R"))
source(GDAL_mosaic_tile)

# Get i parameter from bash script running via slurm (mosaic_pred_tiles.sh)
i <- as.numeric(commandArgs(trailingOnly = TRUE))

categories <- 190
types <- "eval"
# types <- "fit"

my_ids <- crossing(
  types,   categories) %>% 
  mutate(
    ids = str_c(types, "_", categories)
  )

# my_ids
type <- pull(my_ids[i, 1])

category <- pull(my_ids[i, 2])

category_id <- pull(my_ids[i, 3])

pred_outfolder <- str_glue("{datafolder_path}/Data_NL/Model_output/Predictions/Tiled_{category_id}_new")
out_folder_path <- paste0(pred_outfolder, "/*.tif")

outfile <- str_glue("{datafolder_path}/Data_NL/Model_output/Prediction_merged_{category_id}_new_may.tif")

# file.remove(outfile )

if (!file.exists(outfile))
{
  
  GDAL_mosaic_tile(output_file = outfile, folder_path = out_folder_path, large_tif = TRUE)
  
}

# mosaic_may <- 1- raster(str_glue("{datafolder_path}/Data_NL/Model_output/Prediction_merged_eval_190_new_may.tif"))
# plot(mosaic_may)

# mosaic_may <- raster(str_glue("{datafolder_path}/Data_NL/Model_output/Prediction_merged_eval_190_new_may.tif"))

# mosaic_april <- 1- raster(str_glue("{datafolder_path}/Data_NL/Model_output/Prediction_merged_eval_190_new_april.tif"))
# plot(mosaic_april)

# test_mosaic <- raster(str_glue("{datafolder_path}/Data_NL/Model_output/Prediction_merged_eval_190_new_may_allpred.tif"))
# plot(test_mosaic)

