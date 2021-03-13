### shinydashboard WHO CDCC Deaths 2020: filtered data
# author: Remy
# date: 13/01/2021

# required libraries
library(tidyverse) # data manipulation
library(maptools) # wrld_map shapefile that is used for the leaflet map

# custom plot theme which makes the labels bigger
theme_bigfont <- theme(plot.title = element_text(size=16),
                       axis.text.x= element_text(size=13),
                       axis.text.y= element_text(size=13), 
                       axis.title=element_text(size=15),
                       legend.title = element_text(size = 15),
                       legend.text = element_text(size = 14))

theme_plotlyfont <- theme(plot.title = element_text(size=10),
                       axis.text.x= element_text(size=9),
                       axis.text.y= element_text(size=9), 
                       axis.title=element_text(size=10),
                       legend.title = element_text(size = 10),
                       legend.text = element_text(size = 9))

# load data
cdcc_deathprob <- read_csv("who_2020_data/cdcc_filtered.csv") %>% select(-X1)
# undeployed
doctors <- read_csv("who_2020_data/doctors_filtered.csv") %>% select(-X1)
nurses <- read_csv("who_2020_data/nurses_filtered.csv") %>% select(-X1)
pharmacists <- read_csv("who_2020_data/pharmacists_filtered.csv") %>% select(-X1)

# undeployed
medical_personnel <- nurses %>%
  left_join(doctors, by = c("location", "year")) %>%
  left_join(pharmacists, by = c("location", "year")) %>%
  mutate(location = recode(location,
                           "Russian Federation" = "Russia",
                           "Republic of Korea" = "South Korea",
                           "Democratic People's Republic of Korea" = "North Korea",
                           "Côte d’Ivoire" = "Ivory Coast",
                           "Czechia" = "Czech Republic",
                           "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                           "United States of America" = "United States",
                           "Venezuela (Bolivarian Republic of)" = "Venezuela",
                           "Bolivia (Plurinational State of)" = "Bolivia"))

# recode some country names to match those of the wrld_simpl@data$NAME names
cdcc_deathprob <- cdcc_deathprob %>%
  # rename certain countries for the left join later
  mutate(location = recode(location,
                           "Russian Federation" = "Russia",
                           "Republic of Korea" = "South Korea",
                           "Democratic People's Republic of Korea" = "North Korea",
                           "Côte d’Ivoire" = "Ivory Coast",
                           "Czechia" = "Czech Republic",
                           "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                           "United States of America" = "United States",
                           "Venezuela (Bolivarian Republic of)" = "Venezuela",
                           "Bolivia (Plurinational State of)" = "Bolivia")) %>%
  group_by(year) %>%
  mutate(avg_prob = round(mean(percent), 1))

# load world map shapefile
data("wrld_simpl")

# rename certain countries in shapefile for the left join with the cdcc_deathprob dataset
new_country_names <- wrld_simpl@data %>%
  mutate(NAME = recode(NAME,
                       "Korea, Republic of" = "South Korea",
                       "Korea, Democratic People's Republic of" = "North Korea",
                       "Libyan Arab Jamahiriya" = "Libya",
                       "Cote d'Ivoire" = "Ivory Coast",
                       "French Guiana" = "Guyana"))

# add new names to the shapefile "data" slot
wrld_simpl@data <- new_country_names

# mean difference between year 2016 and 2000 which will be used in an infobox
mean_decrease <- cdcc_deathprob %>%
  select(-avg_prob) %>%
  filter(year %in% c(2000, 2016)) %>%
  pivot_wider(names_from = year, values_from = percent) %>%
  mutate(diff = `2016` - `2000`) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE)) %>% pull() %>% abs()