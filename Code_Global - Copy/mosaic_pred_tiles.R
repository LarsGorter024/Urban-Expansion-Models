##########################################
####  Combine predicted tiles into a mosaic
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, tictoc)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"


GDAL_mosaic_tile <- file.path(str_glue("{folder_path}/Code_Global/GDAL/GDAL_mosaic_tile.R"))
source(GDAL_mosaic_tile)

# Get i parameter from bash script running via slurm (mosaic_pred_tiles.sh)
i <- as.numeric(commandArgs(trailingOnly = TRUE))

categories <- 190
types <- c("eval", "fit")

my_ids <- crossing(
  types,   categories) %>% 
  mutate(
    ids = str_c(types, "_", categories)
  )

# my_ids
type <- pull(my_ids[i, 1])

category <- pull(my_ids[i, 2])

category_id <- pull(my_ids[i, 3])

pred_outfolder <- str_glue("{datafolder_path}/Data_Global/Model_output/Predictions/Tiled_{category_id}_new")
out_folder_path <- paste0(pred_outfolder, "/*.tif")

outfile <- str_glue("{datafolder_path}/Data_Global/Model_output/Prediction_merged_{category_id}_new.tif")

# file.remove(outfile )

if (!file.exists(outfile))
{
  
  GDAL_mosaic_tile(output_file = outfile, folder_path = out_folder_path, large_tif = TRUE)
  
}

# split_pred <- raster(str_glue("{datafolder_path}/Data_Global/Predictors_splitted/Temperature_fnl/tile170_Temperature_fnl.tif"))
# plot(split_pred)

# suit_5m <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_5m_average_eval_190.tif"))
# plot(suit_5m)

# suit_30s <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_30s_average_eval_190.tif"))
# plot(suit_30s)

# suit_30s <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_30s_average_eval_190.tif"))
# plot(suit_30s)


# test_tile <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Predictions/Tiled_eval_190_new/Predicted_tile1561_eval_190.tif"))
# plot(test_tile, col="red")
# plot(raster(outfile))
