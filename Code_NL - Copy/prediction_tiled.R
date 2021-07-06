##########################################
####  Predict model
##########################################
#### | Project name: Urban modeling
#### | What it does: Take set of predictors, split them up in tiles, and predict back
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Load packages -----------------------------------------------------------
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(raster, sp, sf, caret, Rahat, ranger, mapview, scrubr, janitor,
               tidyr, tictoc, PresenceAbsence, tidyverse, fs, data.table)

print(.libPaths())


# Load data ---------------------------------------------------------------

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

i <- as.numeric(commandArgs(trailingOnly = TRUE))


###########
  # Predict on the full model
  type = "eval"
  # type = "fit"
  
 
  tile_num <- str_glue("tile{i}_")
  
 
  print(str_glue("Getting predictors for {tile_num}"))
  tic("Loading predictors")
  
  output_foldername <- str_glue("{datafolder_path}/Data_NL/Predictors_splitted")
  
  my_files <- output_foldername %>%
    fs::dir_ls(type = "file", recursive = TRUE)
  
  # aa <- my_files %>%
  #   str_subset("eval", negate = TRUE) # %>% 
  #   str_subset("wetland_fnl|forest_fnl|crops_fnl|urban_fnl|grassland_fnl|crop_distance_fnl|urban_distance_fnl", 
  #              negate = TRUE)
  
  
  my_files %>%
    as_tibble() %>%
    rename(filepath = 1) %>%
    write_csv(str_glue("{datafolder_path}/Data_NL/split_predictors_lists_FE.csv"))
  
  # There are two "predictors_list" files - one is FE containing tiles with fit/eval data "_lists_FE.csv",
  # other one is the old one without the filename extension
  
  predictors_list <- fread(str_glue("{datafolder_path}/Data_NL/split_predictors_lists_FE.csv"))
  
  # predictors_list
  my_tiles_list <- predictors_list %>% 
    filter(str_detect(filepath, tile_num)) %>% 
    filter(str_detect(filepath ,"eval", negate = TRUE)) %>% 
    filter(str_detect(filepath ,"density_1990", negate = TRUE)) %>% 
    # filter(str_detect(filepath, "wetland_fnl|forest_fnl|crops_fnl|urban_fnl|grassland_fnl|crop_distance_fnl|urban_distance_fnl", 
    # negate = TRUE)) %>% 
    pull()
  print(my_tiles_list)
  

  
  predictor_stack <- stack(my_tiles_list)
  toc()
  print(predictor_stack)
  
  names(predictor_stack) <- names(predictor_stack) %>%
    str_remove(tile_num) %>% 
    str_remove(("_fnl")) %>% 
    str_remove("_fit") %>% 
    str_remove("_2005") 
  

  print(names(predictor_stack))
  
  # Check if predictor is empty
  my_vals <- getValues(predictor_stack[[1]])
  
  
  
  if (!all(is.na(my_vals)))
  {
    
    category = 190
   
    category_id <- str_glue("{type}_{category}")
    
    pred_outfolder <- str_glue("{datafolder_path}/Data_NL/Model_output/Predictions/Tiled_{category_id}_new")
    outfname <- str_glue("{pred_outfolder}/Predicted_{tile_num}{category_id}.tif")
    
    model_outfolder <- str_glue("{datafolder_path}/Data_NL/Model_output/Model_runs")
    
    dir.create(pred_outfolder, showWarnings = FALSE)
    
    if (!file.exists(outfname))
    { 
      # list.files(model_outfolder)
      
      print(str_glue("Running model prediction for {category_id}"))
      model_fname <- str_glue("{model_outfolder}/Fitted_model_ANN_{category_id}.rds")
      tic("Loading model")
      model_nnet <- read_rds(model_fname)
      # names(model_nnet["trainingData"][[1]])[-1] <- substring(names(model_nnet["trainingData"][[1]])[-1],2)
      toc()
      
      
      # for (i in 2000:2100)
      # {
      
      tic(str_glue("Running {tile_num}"))
      

      print("<---------------------")
      print(names(predictor_stack))


      
      # Convert some layers to factorial
      categorical_layer_names <- c("ESA_crops", "ESA_forest","ESA_grassland", "ESA_urban", "ESA_wetland", "Protected_areas")
      lyr_names <- names(predictor_stack)
      
      categorical_layers <- which(lyr_names %in% categorical_layer_names)
      
      # Convert to factorial
      for (j in categorical_layers)
      {
        # print(j)
        predictor_stack[[j]] <- as.factor(predictor_stack[[j]])
      }
      
      ####
      print(str_glue("Predicting for {category_id}"))
      tic("Predicted in")
      set.seed(666)
      print(names(predictor_stack))
      print(names(model_nnet$method))
      pred_tmp <- predict(predictor_stack, model_nnet, type = "prob", na.rm=TRUE)
      print("after predict")
      writeRaster(pred_tmp, outfname)
      toc()
      toc()
      
      
      print(str_glue("File created for {tile_num}, {category_id}"))
    } else {
      print(str_glue("File exists for {tile_num}, {category_id}"))
    }
  }
# }

