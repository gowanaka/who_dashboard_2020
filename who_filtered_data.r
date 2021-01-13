### shinydashboard WHO CDCC Deaths 2020: filtered data
# author: remy
# date: 13/01/2021


# load libraries
library(tidyverse) # data manipulation
library(ggplot2) # data viz
library(wesanderson) # color palettes for plots

# load unfiltered data, and filter then save as a new .csv file
cdcc_deathprob <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/30-70cancerChdEtc.csv") %>%
  rename_all(tolower) %>%
  rename("percent" = "first tooltip",
         "sex" = "dim1",
         "year" = "period")
write.csv(cdcc_deathprob, file = "~/Documents/who_2020_shinydashboard/who_2020_data/cdcc_filtered.csv")

# (undeployed) do the same for the doctors, nurses and pharmacists dataframe
# doctor
doctors <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/medicalDoctors.csv") %>% 
  rename_all(tolower) %>%
  select(-indicator) %>%
  rename("year" = "period",
         "doctors per 10000" = "first tooltip")
write.csv(doctors, file = "~/Documents/who_2020_shinydashboard/who_2020_data/doctors_filtered.csv")
# nurses
nurses <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/nursingAndMidwife.csv") %>% 
  rename_all(tolower) %>% 
  select(-indicator) %>%
  rename("year" = "period",
         "nurses per 10000" = "first tooltip")
write.csv(nurses, file = "~/Documents/who_2020_shinydashboard/who_2020_data/nurses_filtered.csv")
# pharmacists
pharmacists <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/pharmacists.csv") %>% 
  rename_all(tolower) %>% 
  select(-indicator) %>%
  rename("year" = "period",
         "pharmacists per 10000" = "first tooltip")
write.csv(pharmacists, file = "~/Documents/who_2020_shinydashboard/who_2020_data/pharmacists_filtered.csv")

  