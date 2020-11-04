# Read this shape file with the rgdal library. 
library(rgdal)
library(broom)
library(tidyverse)
library(stringr)
library(RColorBrewer)

# LOAD DATA
gemeinde_shp <- readOGR( 
  dsn= "data/VG250_Gemeindegrenzen-shp/" , 
  verbose=FALSE
)

# 'fortify' the data to get a dataframe format required by ggplot2
gemeinde_fortified <- tidy(gemeinde_shp, region = "AGS")


# wrangle age data
age_grp <- readxl::read_xlsx("data/age_grp.xlsx", skip = 7,
                             col_names = c("AGS", "name", "t_less3", 
                                           "t_3_6", "t_6_10", "t_10_15", "t_15_18", 
                                           "t_18_20", "t_20_25", "t_25_30", "t_30_35", 
                                           "t_35_40", "t_40_45", "t_45_50", "t_50_55",
                                           "t_55_60", "t_60_65", "t_65_75", "t_75+", "t_total",
                                           "m_less3", 
                                           "m_3_6", "m_6_10", "m_10_15", "m_15_18", 
                                           "m_18_20", "m_20_25", "m_25_30", "m_30_35", 
                                           "m_35_40", "m_40_45", "m_45_50", "m_50_55",
                                           "m_55_60", "m_60_65", "m_65_75", "m_75+", "m_total",
                                           "w_less3", 
                                           "w_3_6", "w_6_10", "w_10_15", "w_15_18", 
                                           "w_18_20", "w_20_25", "w_25_30", "w_30_35", 
                                           "w_35_40", "w_40_45", "w_45_50", "w_50_55",
                                           "w_55_60", "w_60_65", "w_65_75", "w_75+", "w_total"))

# Code "-" as NA
age_grp[ age_grp == "-" ] <- NA

# Select total only
age_grp <- age_grp %>%
  select("AGS", "name", starts_with("t_"))


age_grp <- age_grp %>%
  mutate(AGS = str_pad(AGS, 8, side = c("right"), pad = "0")) %>%
  select("AGS", "name", starts_with("t_")) %>%
  mutate_at(vars(-c(name, AGS)), as.numeric)

pop_total <- age_grp %>%
  select(AGS, t_total) %>%
  mutate(pop_cut = Hmisc::cut2(t_total, g=9))

gemeinde_fortified <- gemeinde_fortified %>%
  select(-t_total, -pop_cut)

gemeinde_fortified <- gemeinde_fortified %>%
  left_join(. , pop_total, by=c("id"="AGS")) 

# Plot it
ggplot(data = gemeinde_fortified) +
  geom_polygon(aes( x = long, y = lat, group = group, fill = pop_cut), size = 0.2) +
  scale_fill_brewer(palette = "Reds") +
  coord_sf() +
  theme_void() 

gemeinde_shp@data