
Crop_function <- function(infile, outfile, scale)
{
 
  if (scale == "Global")
  {
    lc_no_water <- str_glue("{datafolder_path}/Data_{scale}/lc_2019_crop_function.tif")
    lc_no_water2 <- str_glue("{datafolder_path}/Data_{scale}/lc_2019_crop_function2.tif")
    
    if (!file.exists(lc_no_water2))
    {
      
      mystring <- str_glue("gdal_calc.py -A /vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_Global/Crop_mask_esa_2019.tif --outfile={lc_no_water} --calc='0*(A==210)+1*logical_and(A!=210, A>0)'")
      system(mystring)
      
      mystring2 <- str_glue("gdal_translate -of GTiff -a_nodata 0 {lc_no_water} {lc_no_water2}")
      system(mystring2)
      
    }
    
  
    tic("Setting water to NA")
    
    gdal_call <- str_glue("gdal_calc.py -A {lc_no_water2} -B {infile} --outfile={outfile} --calc='B*A'")
    
    system(gdal_call)
    toc()
    
   
  }
  
  else
  {  
    bound_outpath <- str_glue("{folder_path}/{scale}_bound.gpkg")
    
    print("Loading file")
    boundary <- getData("GADM", country = scale, level = 1) %>% 
      st_as_sf()
    boundary <- boundary[boundary$ENGTYPE_1!="Water body",]
    
    st_write(boundary, bound_outpath, append=FALSE)
    
    tic(str_glue("Masking on {scale}"))
    outfile <- gdalR::GDAL_crop(input_raster = infile, filename = outfile, shapefile_path = bound_outpath,
                                large_tif = TRUE)
    toc()
    
  }
  
}

# lc_2019 <- raster("/vol/milkundata/ESA_landcover/Version_2_1/C3S-LC-L4-LCCS-Map-300m-P1Y-2019-v2.1.1.tif")
# lc_2019 <- raster("/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_NL/NL_data/NL_landcover_2019.tif")

