##########################################
####  Prepare data for model fitting
##########################################
#### | Project name: Urban modeling
#### | Script type: Data loading
#### | What it does: Load predictors and prepare them for modeling
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load packages -----------------------------------------------------------

.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(Rahat, tidyverse, raster, sf, data.table)

folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model"
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"

for (type in c("fit", "eval"))
{
  category = 190

  
  category_id <- str_glue("{type}_{category}")
  
  # Load data ---------------------------------------------------------------
  
  # New folder path
  folder_extracted <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_extracted/Single_files_new") 
  
  if (type =="fit") {
  files_extracted <- folder_extracted %>% 
    list.files(full.names = TRUE) %>% 
    str_subset(category_id) %>% 
    str_subset("Pop_density_1990", negate = TRUE) 
  }
  else{
    files_extracted <- folder_extracted %>% 
      list.files(full.names = TRUE) %>% 
      str_subset(category_id) %>% 
      str_subset("Pop_density_2005", negate = TRUE) 
  }
  
  out_folder <- str_glue("{datafolder_path}/Data_NL/Response_variable/Changes_extracted/Combined_files")
  
  output_filename_changes <- str_glue("{out_folder}/Urbanchanges_{category_id}_data_new.csv")
  print(category_id)
  if (!file.exists(output_filename_changes))
  {
    
    
    for (i in 2:length(files_extracted))
    {
      print(i)
      
      
      if (i == 2)
      {
        file_extracted <- files_extracted[1] %>% 
          fread() %>% 
          dplyr::select(-grid)
        
        file_extracted2 <- files_extracted[i] %>% 
          fread() %>% 
          dplyr::select(-PA)
        
        file_combined <- bind_cols(file_extracted, file_extracted2)
        
      } else {
        
       
        file_extracted2 <- files_extracted[i] %>% 
          fread() %>% 
          dplyr::select(-grid, -PA)
        
        # tic()
        file_combined <- bind_cols(file_combined, file_extracted2)
        
        names(file_combined) <- file_combined %>% 
          names() %>% 
          str_remove(str_glue("_{type}")) %>% 
          str_remove("_2005") %>% 
          str_remove("_1990")
      
        # toc()
      }
    }
    
    
    # output_filename_changes
    print(str_glue("Saving {category_id}"))
    fwrite(file_combined, output_filename_changes)
  }
}
