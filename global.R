## global.R ##
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rgdal) #read Geojson file
library(tigris) #geo_join
library(plotly)
library(shinythemes)
library(DT)
library(scales)
library(htmltools)

# read cleaned dataframe
nyc <- read.csv('nyc property sales 2017.csv', stringsAsFactors = FALSE)

# read geocoded file
sample_geocoded <- read.csv('sample_geocoded.csv', stringsAsFactors = FALSE)

# subsetting for datatable display
subset1 <- nyc %>% select(-residential.units, -commercial.units,-total.units,
                          -tax.class.at.time.of.sale,-date,-apartment.number,-X,-neighborhood)

# read Geojson file
nyc_neighborhoods <- readOGR("neigborhoods.json", 'OGRGeoJSON', verbose = F)

# initialize spatial df for map
district_count = nyc %>% group_by(district) %>% summarise(n=n(),price = median(sale.price))
map_data <- geo_join(nyc_neighborhoods, district_count, "neighborhood", "district")
Map_data_manhattan = map_data[map_data@data$borough == "Manhattan", ]


# create variables for choice
element = c('Volume', 'Median Price');
neighborhood = c('All',unique(nyc[nyc$borough == "Manhattan",]$district))
borough = c('All',unique(nyc$borough))


# # intitialize data frame for donut chart
donutAll = 
  nyc %>% filter(borough == 'Manhattan') %>%
  group_by(building.class.category) %>%
  summarise(n=n(),price=median(sale.price))

donut = 
  nyc %>% filter(borough == 'Manhattan') %>%
  group_by(district,building.class.category) %>% #further bread down by neighborhood
  summarise(n=n(),price=median(sale.price))
  

# #initialzie data frame for bar chart
chartTimeAll = nyc %>% filter(borough == 'Manhattan') %>%
  group_by(date) %>%
  summarise(n=n(),price=median(sale.price),avgPrice=mean(sale.price))

chartTime = nyc %>% filter(borough == 'Manhattan') %>%
  group_by(district,date) %>%
  summarise(n=n(),price=median(sale.price),avgPrice=mean(sale.price))
  
