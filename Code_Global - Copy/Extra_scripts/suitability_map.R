##########################################
####  Make global suitability map
##########################################
#### | Project name: My research project
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: February 12, 2020.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Script setup ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
pacman::p_load(Rahat, tidyverse, raster, tictoc, janitor, sf, ggthemes, 
               colorspace, cowplot, patchwork, mapview,
               rworldmap, rcartocolor, scico)

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

# fullres_layers <- str_glue("{datafolder_path}/Data_{scale}/Model_output/") %>% 
#   list.files(pattern = "cropped.*.tif$", full.names = TRUE)

# aggregated_layers <- str_glue("{datafolder_path}/Data_{scale}/Model_output/") %>% 
#   list.files(pattern = "aggregated.*.april.tif$", full.names = TRUE)

fullres_layers <- raster( str_glue("{datafolder_path}/Data_Global/Model_output/Prediction_merged_eval_190_new.tif"))
  
r <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_10m_average_eval_190_new.tif"))

r_5m <- raster(str_glue("{datafolder_path}/Data_Global/Model_output/Aggregated_layers/Prediction_aggregated_5m_average_eval_190_new.tif"))

# Make a single map -------------------------------------------------------

coasts <- str_glue("{datafolder_path}/Coastlines/Coast.shp") %>% 
  st_read()

world_lines <- coasts %>% 
  mutate(
    id = 1:5
  ) %>% 
  filter(id != 5) %>% 
  st_make_valid() %>% 
  group_by(Id) %>% 
  summarize()

robin <- "+proj=robin"


world_lines_robin <- world_lines %>% 
  st_transform(crs = robin)

# plot(world_lines_robin)


#### 
# Change layer here
r_robin <- r_5m %>% 
  # aggregate(fact = 5) %>% 
  projectRaster(crs = robin) 

# Create polygon for cropping and (slightly wider one) and for mapping 
world_polys_buff <- world_lines_robin %>% 
  st_polygonize() %>% 
  st_buffer(10000)

world_polys <- world_lines_robin %>% 
  st_polygonize()

# Crop extent
r_robin_cropped <- mask(r_robin, world_polys_buff)


r_gg <- r_robin_cropped %>% 
  raster_to_gg() %>% 
  rename(
    layer = 1
  )



# Create graticule for map
gr <- st_graticule(lat = c(-89.9, seq(-80,80,20),89.9))
gr <- st_transform(gr, crs = robin, use_gdal = FALSE)



# Full resolution inset  --------------------------------------------------
# 
# # Function skeleton
# region <- "china"
# category <- "190"
# 
# frame_color <- "purple"
# 
# fullres_r <- fullres_layers %>%
#   str_subset(str_glue("{region}_{category}")) %>%
#   raster()
# 
# tic("to gg")
# fullres_inset_gg <- raster_to_gg(fullres_r) %>%
#   rename(layer = 1)
# toc()
# 
# tic("to plot")
# 
# p_inset <- ggplot() +
#   geom_tile(data = fullres_inset_gg, aes(x = x, y = y, fill = layer)) +
#   scale_fill_carto_c(
#     palette = "SunsetDark", limits = c(0, 1)
#   ) +
#   coord_sf(
#     datum = NA, expand = FALSE
#   ) +
#   theme_map() +
#   theme(
#     legend.position = "none",
#     panel.grid = element_blank(),
#     line = element_blank(),
#     rect = element_blank(),
#     text = element_blank(),
#     panel.border = element_rect(color = frame_color, fill = NA, size = 3)
#   )
# toc()


##################
# Insets ------------------------------------------------------------------
# Set extents for inset maps

my_ext1 <- extent(c(78, 81, 17, 20))
my_ext2 <- extent(c(116, 120, 27, 31 )) #extent(c(-61, -57, -19, -15)) #
my_ext3 <- extent(c(3.360782, 7.227095, 49.687, 53.554584)) #


my_poly_india <- my_ext1 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_set_crs(st_crs(4326))

my_poly_china <- my_ext2 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_set_crs(st_crs(4326))

my_poly_NL <- my_ext3 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_set_crs(st_crs(4326))

###

r_india <- crop(fullres_layers, my_ext1)

r_china <- crop(fullres_layers, my_ext2)

r_NL <- crop(fullres_layers, my_ext3)

##

inset_gg_india <- raster_to_gg(r_india) %>% 
  rename(layer = 1)

p_inset_india <- ggplot() +
  geom_tile(data = inset_gg_india, aes(x = x, y = y, fill = layer)) +
  scale_fill_carto_c(
    palette = "SunsetDark", limits = c(0, 1)
  ) +
  coord_sf(
    datum = NA, expand = FALSE
  ) +
  theme_map() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    text = element_text("India"),
    panel.border = element_rect(color = "grey10", fill = NA, size = 3)
  )

##

inset_gg_china <- raster_to_gg(r_china) %>% 
  rename(layer = 1)

p_inset_china <- ggplot() +
  geom_tile(data = inset_gg_china, aes(x = x, y = y, fill = layer)) +
  scale_fill_carto_c(
    palette = "SunsetDark", limits = c(0, 1)
  ) +
  coord_sf(
    datum = NA, expand = FALSE
  ) +
  # labs(tag = "a)") +
  theme_map() +
  theme(
    plot.tag = element_text(size = 22, family = "Helvetica"),
    legend.position = "none",
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    text = element_text("China"),
    panel.border = element_rect(color = "red", fill = NA, size = 3)
  )

##

inset_gg_NL <- raster_to_gg(r_NL) %>% 
  rename(layer = 1)

p_inset_NL <- ggplot() +
  geom_tile(data = inset_gg_NL, aes(x = x, y = y, fill = layer)) +
  scale_fill_carto_c(
    palette = "SunsetDark", limits = c(0, 1)
  ) +
  coord_sf(
    datum = NA, expand = FALSE
  ) +
  # labs(tag = "a)") +
  theme_map() +
  theme(
    plot.tag = element_text(size = 22, family = "Helvetica"),
    legend.position = "none",
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    text = element_text("Netherlands"),
    panel.border = element_rect(color = "orange", fill = NA, size = 3)
  )

# Main plot ---------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = gr, color = "grey40", size = 0.1) +
  geom_tile(data = r_gg, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = world_polys, fill = NA, color = "grey30", size = 0.25) +
  geom_sf(data = my_poly_china, fill = NA, color = "red", size = 1) +
  geom_sf(data = my_poly_india, fill = NA, color = "grey10", size = 1) +
  geom_sf(data = my_poly_NL, fill = NA, color = "orange", size = 1) +
  scale_fill_carto_c(
    palette = "SunsetDark", limits = c(0.001, 1)
     ) +
  coord_sf(
    datum = NA, expand = FALSE
  ) +
  theme_map() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    line = element_blank(),
    rect = element_blank(),
    text = element_blank()
    )



# Legend ------------------------------------------------------------------

my_gg_legend <- p +
  scale_fill_carto_c(
    palette = "SunsetDark", limits = c(0, 1),
    labels = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  guides(
    limits = c(0, 1),
    fill = guide_colorbar(
      frame.colour = "black",
      ticks.colour = "white",
      barwidth = grid::unit(300, "pt"),
      barheight = grid::unit(20, "pt"),
      title = "Urban conversion suitability",
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
  theme(
    legend.position = "bottom",
    legend.spacing.x = grid::unit(3, "pt"),
    legend.margin = margin(l = 0.4, unit='cm'),
    legend.title = element_text(size = 26, family = "Helvetica", vjust = -1, color = "grey10")
  )


my_legend <- get_legend(my_gg_legend)

#### Test plot outlay

# Assemble plot -----------------------------------------------------------


bottom_part <- ((p_inset_india | p_inset_china | p_inset_NL) / my_legend) + 
  plot_layout(
    widths = unit(c(22, 22, 22), c("cm", "cm", "cm"))
    # heights = unit(c(10.5, 10.5, 2), c("cm", "cm", "cm"))
    )


my_plot <- p / bottom_part  + 
  plot_layout(heights = c(4, 4))

# ggsave(milkunize2("mymap.png"), my_plot, dpi = 300,
#        height = 14, width = 22)

###############
###########
ggsave(str_glue("{datafolder_path}/Data_Global/Model_output/Figures/mysuitmap.png"),
  my_plot, dpi = 300,
       height = 14, width = 22)


######################

##########
# left_side <- ((p_inset_india | p_inset_china | p_inset_NL) / my_legend)  + 
#   plot_layout(
#     # widths = unit(c(3, 3, 1), c("cm", "cm", "cm")),
#               heights = unit(c(10.5, 10.5, 2), c("cm", "cm", "cm")))
# 
# 
# my_plot <- (left_side | p)  + 
#   plot_layout(widths = c(0.5, 2)) +
#   plot_annotation(title = str_glue("Urban suitability"),
#                   theme = theme(
#                     plot.title = element_text(size = 26, family = "Helvetica",
#                                               color = "grey10", hjust = 0.5, vjust = -1)
#                   ))
# 
# 
# ggsave(str_glue("{datafolder_path}/Data_Global/Model_output/Figures/Composite.png"), my_plot, dpi = 300,
#        height = 14, width = 22)

####################################################################################################################################################

# !!! OLD !!! ------------------------------------------------------------------



# Make inset map ----------------------------------------------------------
# 
# 
# inset_layers <- str_glue("{datafolder_path}/Data_{scale}/Model_output/") %>% 
#   list.files(pattern = "cropped.*.tif$", full.names = TRUE)
# 
# inset_raster <- inset_layers %>% 
#   str_subset(str_glue("_10")) %>% 
#   str_subset("china") %>% 
#   raster()
# 
# 
# p_inset_india_10 <- make_inset("asia", "10", frame_color = "purple")
#   
# 
# ggsave(p_inset_india_10)
# 
# ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/inset_cat10_india.png"), p_inset_india_10, dpi = 150,
#        height = 14, width = 14)

# inset map function ------------------------------------------------------

# make_inset <- function(region, category, frame_color = "", save = FALSE)
# {
#   
#   inset_raster <- inset_layers %>% 
#     str_subset(str_glue("_{category}")) %>% 
#     str_subset(region) %>% 
#     raster()
#   
#   inset_gg <- raster_to_gg(inset_raster) %>% 
#     rename(layer = 1)
#   
#   p_inset <- ggplot() +
#     geom_tile(data = inset_gg, aes(x = x, y = y, fill = layer)) +
#     scale_fill_carto_c(
#       palette = "SunsetDark", limits = c(0, 1)
#     ) +
#     coord_sf(
#       datum = NA, expand = FALSE
#     ) +
#     theme_map() +
#     theme(
#       legend.position = "none",
#       panel.grid = element_blank(),
#       panel.border = element_rect(color = frame_color, fill = NA, size = 2),
#       line = element_blank(),
#       rect = element_blank(),
#       text = element_blank()
#     )
#   
#   if (save)
#   {
#     ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/inset_cat_{region}_{category}.png"), p_inset, dpi = 150,
#            height = 14, width = 14)
#   }
#   
#   return(p_inset)
# }
# 
# 
# 
# 
# 
# 





############################################################################################################################################################
#### Old below (orange peel stuff)

#########################################
# p_plot <- ggplot(world_sf) +
#   geom_tile(data = lyr_gg, aes(x = x, y = y, fill = layer)) +
#   # scico::scale_fill_scico(palette = 'bamako', direction = -1, begin = 0.3) +
#   scale_fill_ferm(type = "seq", palette = 3, direction = 1, limits = c(0, 1)) +
#   geom_sf(fill = NA, color = "grey30", alpha = 0.15, size = 0.25) +
#   geom_sf(data = goode_without, fill = "white", color = NA) +
#   geom_sf(data = goode_outline, fill = NA, color = "grey30", size = 0.25) +
#   scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
#   scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
#   coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = crs_goode, ndiscr = 1000) + 
#   guides(
#     # title = "",
#     fill = guide_colorbar(
#       title = "",
#       ticks.colour = "grey10",
#       ticks.linewidth = 1.5,
#       barwidth = 90, barheight = 2,
#       nbin = 100,
#       draw.ulim = TRUE,
#       draw.llim = TRUE,
#       label.theme = element_text(
#         size = 18,
#         color = "grey10",
#         family = "Helvetica-Narrow"
#       ))) +
#   labs(title = name
#        # caption = "@mirzacengic | itsprettydata.com"
#   ) +
#   theme(
#     panel.background = element_rect(
#       # fill = "#6babe3",
#       fill = "#a6d5ff",
#       color = "white", size = 1),
#     panel.grid.major = element_line(color = "gray10", size = 0.25),
#     axis.ticks = element_line(color = "gray10", size = 0.25),
#     legend.position = "bottom",
#     plot.caption = element_text(size = 14, family = "ubuntu", 
#                                 color = "grey40", hjust = 0.97),
#     plot.title = element_text(size = 32, family = "Helvetica-Narrow",
#                               color = "grey10", hjust = 0.5)
#   )
# 
# 
# # Load data ---------------------------------------------------------------
# 
# #### Global
# ####
# crs_goode <- "+proj=igh"
# 
# eckert_vi <- "+proj=eck6 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
# 
# # projection outline in long-lat coordinates
# lats <- c(
#   90:-90, # right side down
#   -90:0, 0:-90, # third cut bottom
#   -90:0, 0:-90, # second cut bottom
#   -90:0, 0:-90, # first cut bottom
#   -90:90, # left side up
#   90:0, 0:90, # cut top
#   90 # close
# )
# 
# longs <- c(
#   rep(180, 181), # right side down
#   rep(c(80.01, 79.99), each = 91), # third cut bottom
#   rep(c(-19.99, -20.01), each = 91), # second cut bottom
#   rep(c(-99.99, -100.01), each = 91), # first cut bottom
#   rep(-180, 181), # left side up
#   rep(c(-40.01, -39.99), each = 91), # cut top
#   180 # close
# )
# 
# goode_outline <- 
#   list(cbind(longs, lats)) %>%
#   st_polygon() %>%
#   st_sfc(
#     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   ) %>% 
#   st_transform(crs = crs_goode)
# 
# # bounding box in transformed coordinates
# xlim <- c(-21945470, 21963330)
# ylim <- c(-9538022, 9266738)
# 
# goode_bbox <- 
#   list(
#     cbind(
#       c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
#       c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
#     )
#   ) %>%
#   st_polygon() %>%
#   st_sfc(crs = crs_goode)
# 
# # area outside the earth outline
# goode_without <- st_difference(goode_bbox, goode_outline)
# 
# library(ggmap)
# world_sf <- sf::st_as_sf(rworldmap::getMap(resolution = "low")) %>% 
#   mutate(cont = as.character(continent)) %>% 
#   filter(cont != "Antarctica")
# pred_list <- str_glue("{datafolder_path}/Data_{scale}/Model_output/") %>% 
#   list.files(pattern = "aggregated", full.names = TRUE)
# 
# aggregate(fact = 10, fun = max) %>%
# mask(world_sf) %>% 
#   projectRaster(crs = crs_goode) %>% 
#   mask(as(goode_outline, "Spatial")) %>% 
#   raster_to_gg()
# 
# r_10 <- raster(pred_list[1]) %>% 
#   aggregate(fact = 10, fun = max) %>%
#   mask(world_sf) %>% 
#   projectRaster(crs = crs_goode) %>% 
#   mask(as(goode_outline, "Spatial")) %>% 
#   raster_to_gg()
# 
# r_30 <- raster(pred_list[2])
# r_40 <- raster(pred_list[3])
# 
# r_10 <- r_10 %>% 
#   rename(layer = 1)
# 
# 
# 
# 
# # head(r_10)
# 
# p_190 <- plot_map(data = r_10, name = "Suitability for conversion into homogeneous crops")
# 
# 
# ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/Suitability_map_190.png"), 
#        p_inset_amazon,
#        dpi = 150, height = 14, width = 14)
# 
# 
# 
# ######################
# # Define function
# plot_map <- function(data, name)
# {
#   
#   # tic()
#   lyr_gg <- data# %>% 
#   
#   # toc()
#   
#   p_plot <- ggplot(world_sf) +
#     geom_tile(data = lyr_gg, aes(x = x, y = y, fill = layer)) +
#     # scico::scale_fill_scico(palette = 'bamako', direction = -1, begin = 0.3) +
#     scale_fill_ferm(type = "seq", palette = 3, direction = 1, limits = c(0, 1)) +
#     geom_sf(fill = NA, color = "grey30", alpha = 0.15, size = 0.25) +
#     geom_sf(data = goode_without, fill = "white", color = NA) +
#     geom_sf(data = goode_outline, fill = NA, color = "grey30", size = 0.25) +
#     scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
#     scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
#     coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = crs_goode, ndiscr = 1000) + 
#     guides(
#       # title = "",
#       fill = guide_colorbar(
#         title = "",
#         ticks.colour = "grey10",
#         ticks.linewidth = 1.5,
#         barwidth = 90, barheight = 2,
#         nbin = 100,
#         draw.ulim = TRUE,
#         draw.llim = TRUE,
#         label.theme = element_text(
#           size = 18,
#           color = "grey10",
#           family = "Helvetica-Narrow"
#         ))) +
#     labs(title = name
#          # caption = "@mirzacengic | itsprettydata.com"
#     ) +
#     theme(
#       panel.background = element_rect(
#         # fill = "#6babe3",
#         fill = "#a6d5ff",
#         color = "white", size = 1),
#       panel.grid.major = element_line(color = "gray10", size = 0.25),
#       axis.ticks = element_line(color = "gray10", size = 0.25),
#       legend.position = "bottom",
#       plot.caption = element_text(size = 14, family = "ubuntu", 
#                                   color = "grey40", hjust = 0.97),
#       plot.title = element_text(size = 32, family = "Helvetica-Narrow",
#                                 color = "grey10", hjust = 0.5)
#     )
#   
#   return(p_plot)
# }
# 
# names(r) <- "layer"
# 
# tic()
# agri_lyr_gg <- r %>% 
#   # aggregate(fact = 10, fun = max) %>% 
#   mask(world_sf) %>% 
#   projectRaster(crs = crs_goode) %>% 
#   mask(as(goode_outline, "Spatial")) %>% 
#   raster_to_gg()
# toc()
# # ndvi_mean_gg$layer <- ndvi_mean_gg$layer / 10000
# 
# # summary(ndvi_mean_gg$layer)
# 
# # ndvi_mean_gg$layer[1] <- 1
# 
# p_mean <- ggplot(world_sf) +
#   geom_tile(data = agri_lyr_gg, aes(x = x, y = y, fill = layer)) +
#   # scico::scale_fill_scico(palette = 'bamako', direction = -1, begin = 0.3) +
#   scale_fill_distiller(type = "seq", palette = 3, direction = 1, limits = c(0, 1)) +
#   geom_sf(fill = NA, color = "grey30", alpha = 0.15, size = 0.25) +
#   geom_sf(data = goode_without, fill = "white", color = NA) +
#   geom_sf(data = goode_outline, fill = NA, color = "grey30", size = 0.25) +
#   scale_x_continuous(name = NULL, breaks = seq(-120, 120, by = 60)) +
#   scale_y_continuous(name = NULL, breaks = seq(-60, 60, by = 30)) +
#   coord_sf(xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE, crs = crs_goode, ndiscr = 1000) + 
#   guides(
#     # title = "",
#     fill = guide_colorbar(
#       title = "",
#       ticks.colour = "grey10",
#       ticks.linewidth = 1.5,
#       barwidth = 60, barheight = 1.75,
#       nbin = 100,
#       draw.ulim = TRUE,
#       draw.llim = TRUE,
#       label.theme = element_text(
#         size = 18,
#     color = "grey10",
#     family = "Helvetica-Narrow"
#   ))) +
#   labs(title = "Agricultural conversion suitability - 40"
#        # caption = "@mirzacengic | itsprettydata.com"
#        ) +
#   theme(
#     panel.background = element_rect(fill = "#6babe3", color = "white", size = 1),
#     panel.grid.major = element_line(color = "gray10", size = 0.25),
#     axis.ticks = element_line(color = "gray10", size = 0.25),
#     legend.position = "bottom",
#     plot.caption = element_text(size = 14, family = "ubuntu", 
#                                 color = "grey40", hjust = 0.97),
#     plot.title = element_text(size = 64, family = "AvantGarde",
#                               color = "grey30", hjust = 0.5)
#   )
# 
# 
# 
# ggsave(str_glue("C:/Users/gorte/Documents/Studie/Master/MSc STAGE/Outputs/Suitability_map_40.png"), p_mean,
#        dpi = 200, height = 14, width = 22)

