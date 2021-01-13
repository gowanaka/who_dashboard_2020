### map prep
# author: remy
# date: 13/01/2021

# load the libraries
library(tidyverse) # general manipulation
library(maptools) # for the "wrld_simpl" shp file (holds the chloropleth map)
library(leaflet) # create interactive maps
library(RColorBrewer) # color plaettes for the chloropleths

# load the shp file of all countries from maptools
data("wrld_simpl")

# load the cdcc WHO dataset without the introduced "X1" and unnecessary "indicator" column
cdcc_deathprob <- read_csv("~/Documents/who_2020_shinydashboard/who_2020_data/cdcc_filtered.csv") %>%
 select(-X1, -indicator)

# filter the dataset for the particular values I want the app to filter (test filter)
cdcc_deathprob <- cdcc_deathprob %>%
  filter(year == 2016,
         sex == "Both sexes")

# check the "data" slot; it is a dataframe that contains different geographical info about all countries
head(wrld_simpl@data)

# left join our data into this data slot on the "NAME" column
map_cdcc_data <- wrld_simpl@data %>%
  left_join(cdcc_deathprob, by = c("NAME" = "location"))

# check 
head(map_cdcc_data)

# add this nex dataframe back to the data slot of the shp file
wrld_simpl@data <- map_cdcc_data

# check whether polygons will map to leaflet
wrld_simpl %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()

# create color palette to color countries by death probability
cdcc_pal <- colorNumeric("RdYlGn", domain = wrld_simpl@data$percent, reverse = TRUE, na.color = "#939597")

# create the base map
wrld_simpl %>%
  # set the minimum zoom to 1
  leaflet(options = leafletOptions(minZoom = 1)) %>%
  # add the OSM tiles
  addTiles() %>%
  # set the viewers boundaries
  setMaxBounds(lng1 = max(wrld_simpl@data$LON) + 10,
               lat1 = max(wrld_simpl@data$LAT) + 10,
               lng2 = min(wrld_simpl@data$LON) - 10,
               lat2 = min(wrld_simpl@data$LAT) - 10) %>%
  # set boundary thickness to 1 and color polygons
  addPolygons(weight = 1, 
              color = ~cdcc_pal(percent),
              # add labels that display mean income
              label = ~paste0("Country: ", NAME,
                "Probability: ", label_percent()(percent/100)),
              # highlight polygons on hover
              highlight = highlightOptions(weight = 5, color = "white",
                                           bringToFront = TRUE))

