##########################################
####  Script title
##########################################
#### | Project name: My research project
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: April 13, 2020.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Script setup ------------------------------------------------------------
# .libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
.libPaths("C:/Users/gorte/Documents/R/win-library/4.0")

# devtools::install_github("eliocamp/tagger")
# install.packages("lwgeom")
pacman::p_load(Rahat, tidyverse, raster, sf, patchwork, scales, janitor, scales, ggthemes, cowplot, tictoc, lwgeom, stringr, tagger, ggsci) #, devtools) #

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


# Load data  --------------------------------------------------------------
# my_r <- "Projects/Agriculture_modeling/Data/Model_output" %>% 
#   milkunize2("archive") %>% 
#   list.files(pattern = "aggregated.*.april.tif$", full.names = TRUE) %>% 
#   str_subset("10m_eval_10") %>% 
#   raster()
# 
my_r <-raster("/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_10m_average_eval_190_new.tif")

my_r_agg <- aggregate(my_r, fact = 10)

####
# Make 3x2 map of the response variable


my_files <- str_glue("{datafolder_path}/Data_Global/Response_variable/Changes_vector/") %>% 
  list.files(recursive = TRUE, pattern = "300m.gpkg", full.names = TRUE)


world_lines <- str_glue("{datafolder_path}/Coastlines/Coast.shp") %>% 
  st_read() %>% 
  mutate(
    id = 1:5
  ) %>% 
  filter(id != 5) %>% # Remove Antarctica
  st_make_valid() %>% 
  group_by(Id) %>% 
  summarize()

robin <- "+proj=robin"


world_polys <- world_lines %>% 
  st_transform(crs = robin) %>% 
  st_polygonize()

world_lines_robin <- world_polys %>% 
  st_buffer(10000)

# plot(world_lines_robin[1])
rast <- raster(xmn= -180, ymn= -90, xmx = 180, ymx = 90, 
               # resolution = 0.5,
               resolution = 1,
               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

rast[is.na(rast)] <- 0

# Create graticule for map
gr <- st_graticule(lat = c(-89.9, seq(-80,80,20),89.9))
gr <- st_transform(gr, crs = robin, use_gdal = FALSE)



# Old code ----------------------------------------------------------------
####
# my_files <- "Projects/Agriculture_modeling/Data/Response_variable/Changes_vector" %>% 
#   milkunize2("archive") %>% 
# list.files(pattern = ".grids.shp$", full.names = TRUE)

# fullres_layers <- str_glue("{datafolder_path}/Data_{scale}/Model_output/") %>% 
#   list.files(pattern = "cropped.*.tif$", full.names = TRUE)

# fullres_r <- raster(fullres_layers[4])
# 
# fullres_r
# tic("To point")
# my_300m_raster <- rasterToPoints(fullres_r, spatial = TRUE) %>% 
#   st_as_sf()
# toc()


# write_rds(my_300m_raster, milkunize2("Projects/Agriculture_modeling/temp_300m_response.rds", "archive"))
# 
# sf::st_write(my_300m_raster, milkunize2("Projects/Agriculture_modeling/temp_300m_response.gpkg", "archive"))
# 
# 
# 
# my_var <- milkunize2("Projects/Agriculture_modeling/temp_300m_response.rds", "archive") %>% 
#   read_rds()



# Loop --------------------------------------------------------------------
# i = 1

for (i in seq_along(my_files))
{
  params <- my_files[i] %>% 
    str_remove("/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data/Data_Global/Response_variable/Changes_vector/") %>% 
    str_split("/", simplify = TRUE) %>% 
    as_tibble() %>% 
    transmute(
      type = V2,
      category = "190"
    )
  
  
  outfile_name <- str_glue("{datafolder_path}/Data_Global/Model_output/Figures/figdata/response_var_robin_cropped-{tolower(params$type)}_{params$category}_1deg.csv")
  
  if (!file.exists(outfile_name))
  {

    tic("Loading")
    my_response <- st_read(my_files[i])
    toc()
    
    
    my_presences <- my_response %>% 
      rename(PA = 1) %>% 
      filter(PA == 1)
    
    tic("Rasterizing")
    my_response_raster <- raster::rasterize(my_presences, rast, field = 1, fun = "count")
    toc()
    
    tic("Project and crop")
    my_response_raster_robin <- my_response_raster %>% 
      projectRaster(crs = robin, method = "ngb") %>% 
      mask(world_lines_robin)
    toc()
    
    my_response_gg <- my_response_raster_robin %>% 
      raster_to_gg() %>% 
      rename(
        value = 1
      ) %>%
      mutate(
        percentage = (value / 32400) * 100
      )
    
    
    
    
    
    write_csv(my_response_gg, outfile_name)
  } else {
  my_response_pct_gg <- read_csv(outfile_name)
  }
}


# Make map function -------------------------------------------------------

# ggdraw(p_legend)

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# Define function for making maps

make_map <- function(data, label = "")
{
  
  data_ctg <-
    data %>% 
    mutate(
      pct_disct = cut(percentage, 
                      # c(0, 2.5, 5, 
                      c(0, 
                        # 1, 
                        2.5, 5, 7.5,
                        10, 12.5, 15, 17.5, 20, 25, 100))
    )
  
  p_map <- ggplot() +
    geom_sf(data = gr, color = "grey40", size = 0.1) +
    # geom_tile(data = data, aes(x = x, y = y, fill = percentage)) +
    geom_tile(data = data_ctg, aes(x = x, y = y, fill = pct_disct)) +
    scale_fill_viridis_d(
      option = "magma",
      guide = "colorsteps",
      direction = -1, 
      # labels = scales::label_number(suffix = "%"),
      labels  = function(x) ifelse(is.wholenumber(x), paste0(sprintf("%.0f", x),"%"), paste0(x,"%")),
      # values = c(0, 100),
      # limits = c(0, 1, 2.5, 5, 7.5,
      #            10, 12.5, 15, 17.5, 20, 25, 100)
      # breaks = c(0, 1, 2.5, 5, 7.5,
      # 10, 12.5, 15, 17.5, 20, 25, 100)
    ) +
    # scale_fill_viridis_c(direction = -1,
    #                      # limits = c(0, 50),
    #                      # option = "magma",
    #                      breaks = c(0, 2.5, 5, 7.5, 10, 15, 20),
    #                      # option = "magma",
    #                      label = number_format(suffix = "%"),
    #                      guide = "colorsteps",
    #                      name = "Percentage of cell converted") +
    geom_sf(data = world_polys, fill = NA) +
    # geom_sf(data = my_presences, alpha = 0.5, size = 0.05, color = "red") +
    # scale_color_distiller(type = "seq", palette = 5, direction = 1,
    # name = "Number of conversion per cell") +
    coord_sf(datum = NA, expand = FALSE) +
    theme_map() +
    # ggtitle(label) +
    tagger::tag_facets(tag_pool = label) +
    theme(legend.position = "none",
          # plot.title = element_text(size = 32, family = "Helvetica"),
          plot.title = element_blank(),
          tagger.panel.tag.text = element_text(color = "grey10", size = 38, face = "bold", family = "Helvetica"),
          legend.title = element_text(size = 28),
          panel.grid = element_blank(),
          line = element_blank(),
          rect = element_blank(),
          text = element_blank())
  
  p_map
}


# Read CSV files ----------------------------------------------------------

my_csv_files <- str_glue("{datafolder_path}/Data_{scale}/Model_output/Figures/figdata/") %>% 
  list.files(full.names = TRUE, pattern = "crop*.*.csv")


my_response_pct_gg_e190 <- read_csv(my_csv_files[1])
my_response_pct_gg_f190 <- read_csv(my_csv_files[2])

respmap_e190 <- make_map(my_response_pct_gg_e190, label = "a")
respmap_f190 <- make_map(my_response_pct_gg_f190, label = "b")

cat_190 <- (respmap_e190 + theme(plot.margin = margin(r = 0.5, unit='cm')) | respmap_f190)


# Make map legend ---------------------------------------------------------

p_raster <- make_map(my_response_pct_gg_e190, label = "a)") +
  guides(
    # guide_colo
    fill = guide_colorsteps(
      frame.colour = "white",
      ticks.colour = "white",
      limits = c(0, 100),
      ticks = TRUE,
      # labels = scales::percent,
      show.limits = TRUE,
      barwidth = grid::unit(1100, "pt"),
      # barwidth = grid::unit(600, "pt"),
      barheight = grid::unit(30, "pt"),
      title.position = "top",
      ticks.linewidth = 3,
      label.theme = element_text(
        size = 28,
        hjust = 0.5,
        vjust = 1,
        color = "grey20",
        family = "Helvetica"
      )
    )
  ) +
  labs(fill = "Percentage of cell covered by the response variable") +
  theme(legend.position = "bottom",
        legend.margin = margin(l = 8.5, t = 1,unit='cm'),
        # legend.title = element_blank(),
        legend.title = element_text(size = 32, color = "grey10", family = "Helvetica", vjust = -0.5, hjust = 0.5),
  )
# make map legend ---------------------------------------------------------

p_legend <- get_legend(p_raster)
# ggdraw(p_legend)


my_plots <- respmap_e190 / respmap_f190 / p_legend + #cat_190 / p_legend +
  plot_layout(
    # widths = unit(c(13, 13, 3), c("cm", "cm", "cm")),
    heights = unit(c(14, 14, 4), c("cm", "cm", "cm", "cm"))
  )



tic()

ggsave(str_glue("{datafolder_path}/Data_Global/Model_output/Figures/Response_test.jpg"), my_plots,
       dpi = 300, width = 22, height = 20)
toc()



###################################################################################################################################################

my_response_pct_gg_e190_mod <- my_response_pct_gg_e190 %>% 
  mutate(
    pct_disct = cut(percentage,   c(0, 1, 2.5, 5, 7.5,
                                    10, 12.5, 15, 17.5, 20, 25, 100))
  )

pp <- ggplot() +
  geom_tile(data = my_response_pct_gg_e190_mod, aes(x = x, y = y, fill = pct_disct)) +
  scale_fill_viridis_d(
    guide = "colorsteps",
    direction = -1,
   
  ) +
  guides(

    fill = guide_colorsteps(
      frame.colour = "black",
      ticks.colour = "white",
      barwidth = grid::unit(800, "pt"),
      barheight = grid::unit(30, "pt"),
      title.position = "top",
      ticks.linewidth = 3,
      label.theme = element_text(
        size = 22,
        hjust = 0.5,
        vjust = 0.5,
        color = "grey20",
        family = "Helvetica"
      )
    )
  ) +
  theme_map() +
  theme(legend.position = "bottom",
        # legend.title =element_blank(),
        legend.margin = margin(l = 0, unit='cm'),
        legend.title = element_text(size = 24, family = "Helvetica")
  )



ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/maptest.png"), pp, 
       dpi = 100, width = 12, height = 8)





########################################################################################
########################################################################################
# Load shapefile with coasts (for map outline)


# Set proj4string for Robinson projection
robin <- "+proj=robin"

# Transform to robin projection
world_lines_robin <- world_lines %>% 
  st_transform(crs = robin)

# Create polygon for cropping and (slightly wider one) and for mapping 
world_polys_buff <- world_lines_robin %>% 
  st_polygonize() %>% 
  st_buffer(10000)
# Convert line to polygon

# gr <- st_transform(gr, crs = robin, use_gdal = FALSE)

# Make map ----------------------------------------------------------------

p_points_alpha <- ggplot() +
  geom_sf(data = gr, color = "grey40", size = 0.1) +
  # geom_tile(data = my_response_gg, aes(x = x, y = y, fill = value)) +
  # scale_fill_fermenter(palette = 4, direction = 1) +
  geom_sf(data = world_polys, fill = NA) +
  geom_sf(data = my_presences, alpha = 0.5, size = 0.05, color = "red") +
  scale_color_distiller(type = "seq", palette = 5, direction = 1) +
  coord_sf() +
  theme_map()


tic()
ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/response_points_alpha.png"), p_points_alpha, dpi = 250,
       height = 14, width = 14)
toc()


tic("Rasterizing")
my_response_raster <- raster::rasterize(my_presences, my_r, field = 1, fun = "count")
toc()

# plot(my_response_raster)

my_response_raster_agg <- raster::rasterize(my_presences, my_r_agg, field = 1, fun = "count")
my_response_sp <- rasterToPoints(my_response_raster_agg, spatial = TRUE) %>% 
  st_as_sf()


class(my_response_gg)
extent(gr)
extent(my_response_raster)
my_response_raster <- projectRaster(my_response_raster,crs=robin)
my_response_gg <- raster_to_gg(my_response_raster) 
high_values <- my_response_gg[my_response_gg$layer>500,]
filtered_values <- my_response_gg[my_response_gg$layer<500,]

p_raster <- ggplot() +
  geom_sf(data = gr, color = "grey40", size = 0.1) +
  geom_tile(data = filtered_values, aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(direction = -1, option="C",
                       name = "Number of urban conversion per cell") +
  geom_sf(data = world_polys, fill = NA) +
  # geom_sf(data = my_presences, alpha = 0.5, size = 0.05, color = "red") +
  # scale_color_distiller(type = "seq", palette = 5, direction = 1,
  # name = "Number of conversion per cell") +
  guides(
    fill = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "white",
      barwidth = grid::unit(800, "pt"),
      barheight = grid::unit(40, "pt"),
      title.position = "top",
      ticks.linewidth = 3,
      label.theme = element_text(
        size = 22,
        hjust = 0.5,
        vjust = 0.5,
        color = "grey20",
        family = "Helvetica"
      )
    )
  ) +
  coord_sf() +
  theme_map() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 28))


tic()
ggsave(str_glue("{datafolder_path}/Data_Global/Model_output/Figures/response_raster_4.png"), p_raster, dpi = 250,
       height = 14, width = 14)
toc()


p_points_size <- ggplot() +
  geom_sf(data = gr, color = "grey40", size = 0.1) +
  # geom_tile(data = my_response_gg, aes(x = x, y = y, fill = value)) +
  # scale_fill_fermenter(palette = 4, direction = 1) +
  geom_sf(data = world_polys, fill = NA) +
  geom_sf(data = my_response_sp, aes(size = layer), color = "red") +
  # scale_color_distiller(type = "seq", palette = 5, direction = 1) +
  coord_sf() +
  theme_map()


tic()
ggsave(str_glue("{datafolder_path}/Data_{scale}/Model_output/Figures/response_point_size.png"), p_points_size, dpi = 250,
       height = 14, width = 14)
toc()