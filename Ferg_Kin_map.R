#Febrauary 2022: Maps for dissertation

#setting wd
setwd("~/Library/CloudStorage/OneDrive-Personal/Dissertation/Maps")

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tidyverse)
library(maps)
library(mapproj)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(rgdal)
library(htmltools)
library(htmlwidgets)

#reading in the shapefile by naming the layer in the working directory
muni <-readOGR("2021_Council_District_Apportionment.shp")
fergkin <-readOGR("FergKin.shp")
muni <- spTransform(muni, CRS("+init=epsg:4326")) # Reproject coordinates

#using leaflet for interactive maps
map <-leaflet() %>%
  setView(lat=38.66504, lng =-90.38412, 10) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPopups(lat=38.738254, lng =-90.273423, popup = "Location of Michael Brown Death") %>%
  addPolygons(data = fergkin, color = "#f06262", weight = 1, smoothFactor = 1) %>%
  addControl(rr, position = "center" )
map


#using the tmap package.  Can make the maps interactive or static
#using tmap
fkmap <-tm_shape(fergkin) + tm_polygons(col = "#f06262", border.alpha = 1, lwd = 2, alpha = .3) +
  tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
  tm_compass(position = c("left", "top"), size = 2) +
  tm_layout(title = "Ferguson and Kinloch, MO") +
  tm_layout(title.position = c('center', 'top')) +
  tm_basemap(alpha = 1, server = "CartoDB") +
  tmap_mode('view') #determines if static or interactive
fkmap
print(fkmap, vp = grid::viewport(0.8, 0.85, width = 0.2, height = 0.45))



#using ggmap
register_google(key = "AIzaSyAkeuk6mU8DMSRteuPvF10xF5PWJSjvkM4", write = T)
library(ggmap)
library(ggthemes)
library(ggrepel)
library(ggtext)
library(GISTools)
library(ggsn)
library(ggspatial)
devtools::install_github("3wen/legendMap", force = T)

muni <-readOGR("2021_Council_District_Apportionment.shp")
fergkin <-readOGR("FergKin.shp")
muni <- spTransform(muni, CRS("+init=epsg:4326")) # Reproject coordinates

#map of Kinloch and Ferguson####################################################

Ferg<-get_map("Ferguson, MO", zoom = 13, source = "google", maptype = "roadmap")

main <-ggmap(Ferg) +
  geom_polygon(aes(x = long, y = lat, group=id),
               data = fergkin, color ="black", fill ="dodgerblue", alpha = .4, size = .8) +
  geom_point(aes(y = 38.738254, x = -90.273423), fill = "red", size = 3.5, shape = 23) +
  geom_label(aes(y = 38.742, x = -90.275, label = "Location of \n Michael Brown's Death")) +
  geom_point(aes(y = 38.738, x = -90.325), fill = "red", size = 3.5, shape = 23) +
  geom_label(aes(y = 38.734, x = -90.325, label = "Approximate Location of \n 1962 Unrest")) +
  coord_map() +
  labs(title = "Locations of 1962 and 2014 Police Incidents") +
  theme(plot.title = element_text(face = "bold", size = 14))
main
  ggsave("ferg_kin.png", dpi = 500)

  
#making an inset
  
inset <-get_map("Ferguson, MO", zoom = 10, maptype = "roadmap", source = "google")

inset1<-ggmap(inset) +
      geom_rect(xmin = -90.359, xmax = -90.23, ymin = 38.69, ymax = 38.79, fill = NA, colour = "black", size = 1.0) +
      theme_map()
inset1
  
final_map <- main +
  inset(ggplotGrob(inset1), xmin = -90.300, xmax = -90.231, ymin = 38.70, ymax = 38.72) +
  geom_rect(xmin = -90.277, xmax = -90.254, ymin = 38.70188, ymax = 38.7193, 
            fill = NA, colour = "black", size = .75)
  
final_map
  ggsave("ferg_kin.png", dpi = 500)
  
#creating a map with all police departments as an overview
#reading in the shapefile
pds <-readOGR("allpds_2021.shp")
#transforming the shapefile's projection
pds <- spTransform(muni, CRS("+init=epsg:4326"))

#using Google to get a map of St. Louis County
stlouis<-get_map("St. Louis County, MO", zoom = 10, source = "google", maptype = "roadmap")
ggmap(stlouis)
overview <-ggmap(stlouis) +
  geom_polygon(aes(x = long, y = lat, group=group),
               data = pds, color ="black", fill ="dodgerblue", alpha = .4, size = .5) +
  geom_point(aes(y = 38.738254, x = -90.273423), fill = "red", size = 4.5, shape = 23) +
  geom_label(aes(y = 38.700, x = -90.130, label = "Location of \n Michael Brown's Death")) +
  coord_map() +
  labs(title = "St. Louis County Police Departments") +
  theme(plot.title = element_text(face = "bold", size = 14))

  
overview
  
