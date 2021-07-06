#!/usr/bin/env Rscript

#SBATCH --partition=milkun
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=mirzaceng@gmail.com
#SBATCH --time=44:00:00
#SBATCH --output "Logs/Get_hyperparameters.log"
#SBATCH --mem=196G
#SBATCH -n 16


##########################################
####  Get optimal ANN model hyperparameters
##########################################
#### | Project name: Urban modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load packages -----------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(raster, sp, sf, caret, Rahat, ranger, mapview, scrubr,caret, janitor,
               data.table, tidyr, tictoc, PresenceAbsence, tidyverse, fs, doParallel)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

# Set number of cores
cores_num <- 16


# Define parameters -------------------------------------------------------
category <- 190
type <- "eval"

# for (type in c("fit", "eval"))
# {
  
  category_id <- str_glue("{type}_{category}")
  
  model_outfolder <- str_glue("{datafolder_path}/Data_Global/Model_output/Model_runs")
  model_params_outname <- str_glue("{model_outfolder}/Model_parameters_{category_id}.rds")
  
  mdata_folder <- str_glue("{datafolder_path}/Data_Global/Response_variable/Changes_extracted/Combined_files")
  
  if (!file.exists(model_params_outname))
  {
    
    
    # Load data ---------------------------------------------------------------
    # Load here model data prepared per category and model type
    
    cl <- makePSOCKcluster(cores_num)
    registerDoParallel(cl)
    clusterEvalQ(cl, .libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages"))
    
    input_fname <- str_glue("{mdata_folder}/Urbanchanges_{category_id}_data_new.csv")
    model_data_raw <- fread(input_fname)
    
    # Modeling setup ----------------------------------------------------------
    
    
    
    # Define here what are the limits for seeking optimal size and decay parameters
    hyperparameter_grid <- expand.grid(size = seq(from = 1, to = 20, by = 1),
                                       decay = c(0.05, 0.01, 
                                                 0.005, 0.001,
                                                 0.0005, 0.0001))
    
    train_control <- trainControl(
      method = "cv", 
      number = 10, 
      p = 75,
      verboseIter = FALSE,
      allowParallel = TRUE
    )
    
    # Store results -----------------------------------------------------------
    ###########################################
    
    model_data <- model_data_raw %>% 
      mutate_at(vars(contains("ESA")), ~ifelse(is.na(.), 0, 1)) %>% 
      drop_na() %>% 
      mutate(
        PA = factor(ifelse(PA == 1, TRUE, FALSE), levels = c(TRUE, FALSE))
      )
    
    # Define categorical layers as factor
    categorical_layer_names <- c("ESA.crops", "ESA.forest","ESA.grassland", "ESA.urban", "ESA.wetland", "Protected.areas")
    
    lyr_names <- model_data %>% 
      names()
    
    categorical_layers <- which(lyr_names %in% categorical_layer_names)
    
    # Convert to factorial
    for (i in seq_along(categorical_layers))
    {
      model_data[, categorical_layers[i]] <- as.factor(model_data[, categorical_layers[i]])
    }
    
    model_data_train <- drop_na(model_data)
    # Fit model ---------------------------------------------------------------
    # Run model
    tic(str_glue("Running model for hyperparameter estimation."))
    set.seed(666)
    
    model_nnet <- train(
      PA ~ ., data = model_data_train,
      tuneGrid = hyperparameter_grid,
      trControl = train_control,
      method = "nnet")
    toc()
    stopCluster(cl)
    
    write_rds(model_nnet, model_params_outname)
  }
# }

