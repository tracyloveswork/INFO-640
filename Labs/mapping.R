# Spatial Analysis & Mapping

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("sf")
install.packages("sp")
install.packages("ggthemes")
install.packages("tigris")
install.packages("tmap")
install.packages("leaflet")
install.packages("rgdal")

library(tidyverse)
library(lubridate)
library(leaflet)
library(ggmap)
library(tmap)
library(tigris)
library(sp)
library(ggthemes)
library(maps)
library(mapdata)
library(sf)
library(stringr)
library(rgdal)

# Using the data from NYC Open Data, we are going to look at violations reported from 311 calls
# https://nycopendata.socrata.com/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9

# My API Key
register_google(key = "#", write=TRUE)

# Load a map of NYC by passing along the longitute and latitude 
# On Google Maps: https://www.google.com/maps/@40.6971494,-74.2598655,10z
# The get_map function takes 3 arguments: the location (longitute and latitude), the zoom, and the scale (the resolution of download, the default is 2).

nyc <- c(lon = -73.950, lat = 40.6971)
nyc_map <- get_map(location = nyc, zoom = 10, scale=1)
ggmap(nyc_map)

stamen_nyc_map <- get_stamenmap(bbox = c(left = -74.250, bottom = 40.5, right = -73.450, top = 40.95), zoom = 10, 
              maptype = c("terrain","terrain-background", "terrain-labels", "terrain-lines", "toner",
                          "toner-2010", "toner-2011", "toner-background", "toner-hybrid","toner-labels", 
                          "toner-lines", "toner-lite", "watercolor"),
              crop = TRUE, messaging = FALSE, urlonly = FALSE,
              color = c("color", "bw"), force = FALSE, where = tempdir())
ggmap(stamen_nyc_map)

# Try different zoon levels
nyc_map <- get_map(location = nyc, zoom = 10, scale=1)
nyc_map <- get_map(location = nyc, zoom = 20, scale=1)
nyc_map <- get_map(location = nyc, zoom = 11, scale=1)
ggmap(nyc_map)

# Load CSV of 311 Calls
noise_311 <- read.csv("../DataSets/311_Service_Requests_from_2010_to_present.csv", header=TRUE, check.names = TRUE, stringsAsFactors = FALSE)
glimpse(noise_311)
dim(noise_311)

# Remove columns that are all NA's
not_all_na <- function(x) any(!is.na(x))

noise_311_clean <- noise_311 %>%
  select_if(not_all_na)
  
glimpse(noise_311_clean)

# Covert dates from factor to date/time format with lubridate
noise_311_clean$Created.Date <- mdy_hms(noise_311_clean$Created.Date)
noise_311_clean$Closed.Date <- mdy_hms(noise_311_clean$Closed.Date)
glimpse(noise_311_clean)

# Plot noise calls
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = noise_311_clean)

# Let's look at ice cream truck complaints, this information would be listed in the Descriptor row
unique(noise_311_clean$Descriptor)
as.character(noise_311_clean$Descriptor)

# Create a varaible to hold ones that contain "ice cream"
icecream_311 <- noise_311_clean %>%
  filter(str_detect(Descriptor, "Ice Cream"))

# Check it out
dim(icecream_311)
unique(icecream_311$Descriptor)
glimpse(icecream_311)

ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

#Plot again with great resolutions
nyc_map <- get_map(location = nyc, zoom = 11, scale=2)
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude), data = icecream_311)

# Color by address type
ggmap(nyc_map) +
  geom_point(aes(Longitude, Latitude, color=Address.Type), data = icecream_311)

ggmap(nyc_map,
  base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type)

             
ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Address.Type)) +
               facet_wrap(~Address.Type))

ggmap(nyc_map,
      base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) +
  geom_point(aes(color=Borough)) +
  theme_void()+
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  labs(title="Ice Cream Noise Complaints by Address Type", caption="Source: NYC Open Data") +
  facet_wrap(~Address.Type)









