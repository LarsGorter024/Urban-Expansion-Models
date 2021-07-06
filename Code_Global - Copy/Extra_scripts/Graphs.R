##########################################
####  Graphs
##########################################
#### | Project name: Urban modeling
#### | Creator: Lars Gorter
#### | Contact: l.gorter@student.ru.nl
##########################################

# Load packages -----------------------------------------------------------
.libPaths("C:/Users/gorte/Documents/R/win-library/4.0")
# .libPaths("/vol/milkunB/ES_students/lgorter/Urban_Expansion_model/Rpackages")
pacman::p_load(raster, rgdal, tictoc, sf, fs, glue, tidyverse, mapview, gdalR, glue, ggplot2, ggsci, vroom, xlsx)

# Folder Path -------------------------------------------------------------
scale <- "Global"
local <- Sys.info()["sysname"] == "Windows"

if (local == TRUE)
{ # folder_path <- "R:/ES_students/lgorter/Urban_Expansion_model"
  #datafolder_path <- "X:/ES_students/lgorter/Urban_Expansion_model/Data" 
  folder_path <- "//milkunstud-srv.science.ru.nl/milkunstud/lgorter/Urban_Expansion_model" 
  datafolder_path <- "//milkunstud-srv.science.ru.nl/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data" 
}

if (local != TRUE)
{  folder_path <- "/vol/milkunB/ES_students/lgorter/Urban_Expansion_model" 
datafolder_path <- "/vol/milkunarc/ES_students/lgorter/Urban_Expansion_model/Data"
}

# Load data ---------------------------------------------------------------
setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses"))
csv_path <- str_glue("{datafolder_path}/Data_{scale}/Extra_analyses")


# setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/KBAs_Global"))
# kba_files <- list.files(path = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/KBAs_Global") , pattern = 'KBA')
# KBAs <- na.omit(vroom(kba_files))
# write.csv(KBAs, file = str_glue("{csv_path}/KBAs_global/KBAs_fnl.csv") )
KBAs <- read.csv(str_glue("{csv_path}/KBAs_global/KBAs_fnl.csv"))

# setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_Global"))
# pa_files <- list.files(path = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/PAs_Global") , pattern = 'PA')
# PAs <- na.omit(vroom(pa_files))
# write.csv(PAs, file = str_glue("{csv_path}/PAs_global/PAs_fnl.csv") )
PAs <- read.csv(str_glue("{csv_path}/PAs_global/PAs_fnl.csv"))


# setwd(str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_Global"))
# cropland_files <- list.files(path = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_Global") , pattern = 'Cropland')
# Cropland <- vroom(cropland_files)
# write.csv(Cropland, file = str_glue("{datafolder_path}/Data_{scale}/Extra_analyses/Cropland_Global/Cropland_fnl.csv") )
Cropland <- read.csv(str_glue("{csv_path}/Cropland_Global/Cropland_fnl.csv"))


# Scatterplot relatives per country ---------------------------------------

A <- PAs[,8]/PAs[,9]
B <- PAs[,13]/PAs[,14]

PAs <- cbind(PAs,A,B)

PAMEAN <- PAs %>%
  group_by(continent) %>%
  summarise(
    y= mean(na.omit(A)),
    x= mean(na.omit(B))
  )

## 
C <- KBAs[,7]/KBAs[,8]
D <- KBAs[,12]/KBAs[,13]

KBAs <- cbind(KBAs,C,D)

KBAMEAN <- KBAs %>%
  group_by(continent) %>%
  summarise(
    y= mean(na.omit(C)),
    x= mean(na.omit(D))
  )

##

## 
G <- Cropland[,8]/Cropland[,16]
H <- Cropland[,12]/Cropland[,17]

Cropland <- cbind(Cropland,G,H)

CroplandMEAN <- Cropland %>%
  group_by(continent) %>%
  summarise(
    y= mean(na.omit(G)),
    x= mean(na.omit(H))
  )



# Add size=continent for full global dataset, make continent columns in csv
ggplot(KBAs, aes(y=(area_in_kba_cat_4/total_kba_area), x=(area_in_country_cat_4/total_country_area), color=continent)) +  #
  geom_point()+
  geom_point(data=KBAMEAN, aes(y=y, x=x, color=continent), size=4, shape=17) +
  # geom_smooth(aes(group = 1), method = "loess", se=FALSE, color="black", size=0.5) +
  labs(x = "High suitability for urban area / Total country area",
       title = "",
       subtitle = "",
       y = "High suitability for urban area in KBA / Total KBA area",
       color = "Continent") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


ggplot(PAs, aes(y=(area_in_pa_cat_4/total_pa_area), x=(area_in_country_cat_4/total_country_area), color=continent)) +   #, color=country
  geom_point()+
  geom_point(data=PAMEAN, aes(y=y, x=x, color=continent), size=4, shape=17) +
  # geom_smooth(aes(group = 1), method = "loess", se=FALSE, color="black", size=0.5) +
  labs(x = "High suitability for urban area / Total country area",
       title = "",
       subtitle = "",
       y = "High suitability for urban area in PAs / Total PAs area",
       color = "Continent",
       shape=1) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))



ggplot(Cropland, aes(y=(urban_and_agri_area_country_cat_4/(agri_area_in_country_cat_4)), x=(urban_area_in_country_cat_4/total_country_area), color=continent)) +
  geom_point()+
  geom_point(data=CroplandMEAN, aes(y=y, x=x, color=continent), size=4, shape=17) +
  # geom_smooth(aes(group = 1), method = "loess", se=FALSE, color="black", size=0.5) +
  labs(x = "High suitability for urban area / Total country area",
       title = "",
       subtitle = "",
       y = "High suitability for urban and agri area / High suitability for agri area",
       color = "Country")  +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))




