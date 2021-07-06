##########################################
####  Make variable importance plots
##########################################
#### | Project name: My research project
#### | Script type: Data processing
#### | What it does: Description
#### | Date created: June 04, 2020.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################


# Setup script ------------------------------------------------------------
.libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
# .libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
pacman::p_load(Rahat, tidyverse, janitor, tictoc, data.table, ggsci,  caret)#, tagger)

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


# Define functions --------------------------------------------------------

my_varimp <- read.csv(str_glue("{datafolder_path}/Data_Global/Model_output/Variable_importance/Variable_importance_eval_190.csv"))
# class(my_varimp)
# my_varimp <- as.data.frame(t(my_varimp))
# colnames(my_varimp) <- c("PA","Access","DEM", "Pop_density",	"Precipitation", "Temperature")
# my_varimp <- my_varimp[2:nrow(my_varimp),]

my_plot_data <- my_varimp %>% 
  filter(variable_name != "PA") %>% 
  filter(variable_name != "NA") %>% 
  mutate(
    variable_name = as.character(variable_name),
    Variable_fullname = case_when(
      variable_name == "Temperature" ~ "Annual mean temperature",
      variable_name == "Precipitation" ~ "Annual precipitation",
      variable_name == "Access" ~ "Accessibility",
      # variable_name == "ESA_190_distance" ~ "Distance from urban areas",
      # variable_name == "ESA_210_distance" ~ "Distance from water",
      
      # variable_name == "ESA_crops" ~ "Previous land cover - crops",
      # variable_name == "ESA_forest" ~ "Previous land cover - forest",
      # variable_name == "ESA_grassland" ~ "Previous land cover - grassland",
      # variable_name == "ESA_urban" ~ "Previous land cover - urban",
      # variable_name == "ESA_wetland" ~ "Previous land cover - wetland",
      
      # variable_name == "Protected_areas" ~ "Protected areas",
      # variable_name == "Proposed_protected" ~ "Proposed protected areas",
      # variable_name == "KBAs" ~ "Key biodiversity areas",
      
      variable_name == "Pop_density" ~ "Population density",
      variable_name == "DEM" ~ "Elevation"),
      
      Variable_type =  case_when(
        str_detect(variable_name, "Temperature|Precipitation") ~ "Climate",
        # str_detect(variable_name, "crops|forest|grassland|wetland") ~ "Land-use",
        str_detect(variable_name, "DEM|Access") ~ "Terrain",
        # str_detect(variable_name, "distance") ~ "Distance to",
        str_detect(variable_name, "Pop_dens") ~ "Demographic",
        # str_detect(variable_name, "KBAs|Protected|Proposed") ~ "Environmental",
        TRUE ~ variable_name)
  ) %>%
  pivot_longer(Iter_1:Iter_100, names_to = "iteration") %>% 
  group_by(Variable_type) %>% 
  mutate(
    group_mean = mean(value, na.rm = TRUE)
  ) %>% 
  ungroup()  %>% 
  group_by(Variable_fullname) %>% 
  mutate(
    variable_mean = mean(value, na.rm = TRUE)
  ) %>%
  ungroup()

  
  # group_by(Variable_fullname) %>% 
  # summarize(
  #   variable_mean = mean(value)
  # ) %>% 
  # as.data.table()

############
my_plot_data %>% 
  filter(is.na(Variable_fullname))

# Order the levels
lvl_order <- my_plot_data %>% 
  arrange(desc(group_mean, variable_mean)) %>% 
  distinct(Variable_fullname) %>% 
  pull()

my_plot_data$Variable_fullname <- factor(my_plot_data$Variable_fullname, levels = lvl_order)

############

my_plot_data2 <- my_plot_data %>% 
  mutate(
    Variable_type = case_when(
      Variable_type == "Climate" ~ "Climate",
      # Variable_type == "Land-use" ~ "Land-use",
      Variable_type == "Terrain" ~ "Terrain",
      # Variable_type == "Distance to" ~ "Distance to",
      Variable_type == "Demographic" ~ "Demographic",
      # Variable_type == "Environmental" ~ "Environmental",
    ),
    Variable_type = factor(Variable_type, 
                           levels = c("Climate", "Terrain", "Demographic")) #"Land-use", "Distance to", "Environmental"
  )

#####
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

my_labels <-
  lvl_order %>% 
  as_tibble() %>% 
  mutate(
    labels = case_when(
      value == "Annual mean temperature" ~ "Mean temperature",
      # value == "Previous land cover - forest" ~ "Previous LC - forest",
      # value == "Previous land cover - grassland" ~ "Previous LC - grassland",
      # value == "Previous land cover - crops" ~ "Previous LC - crops",
      # value == "Previous land cover - urban" ~ "Previous LC - urban",
      # value == "Previous land cover - wetland" ~ "Previous LC - wetland",
      # value == "Distance from urban areas" ~ "Distance from urban",
      
      
      TRUE ~ value
    )
  ) %>% 
  pull(labels)


# y was Importance

p_varimp <- my_plot_data2 %>%
  # filter(str_detect("NA")) %>% 
  distinct(Variable_fullname, .keep_all = TRUE) %>% 
  ggplot() + 
  aes(x = Variable_fullname, y = variable_mean , fill = Variable_type) +
  coord_flip() + 
  geom_col(width = 0.5) +
  ylab("Variable importance") +
  scale_fill_jco(
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.05)),
    labels = my_labels
  ) +
  # expand_scale()
  scale_y_continuous(
    labels  = function(x) ifelse(is.wholenumber(x), sprintf("%.0f", x), x),
    limits = c(0, 1)
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, family = "Helvetica", color = "grey30"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(color = "grey50", size = 0.1),
        axis.text.y = element_text(margin = margin(r = 1, l = 0.5, t = 0, b = 0), 
                                   family = "Helvetica", size = 16),
        axis.text.x = element_text(size = 13, color = "grey20", family = "Helvetica"),
        # axis.text.x = element_blank(),
        # axis.line.x = element_blank(),
        axis.ticks.y = element_line(),
        legend.margin = margin(b = 0, t = 0),
        legend.key.size = unit(1.25, "line"),
        legend.text = element_text(size = 18, color = "grey20", family = "Helvetica-Narrow"),
        strip.text = element_text(size = 20, family = "Helvetica", color = "grey10"),
        panel.background = element_rect(fill = NA, color = "grey60"),
        panel.grid.major.x = element_line(linetype = "dotted", color = "grey50")
  ) 



ggsave(str_glue("{datafolder_path}/Data_Global/Model_output/Figures/Figure_varimp_test.jpg"), p_varimp,
       dpi = 300, width = 10, height = 12)  




