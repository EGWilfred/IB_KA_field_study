
library(haven)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)
library(ggplot2)
library(ggmap)
library(plotly)

ggplot(shapeslga)+
  geom_sf(aes(geometry=geometry))


shapeward <- st_read(dsn = "/Users/user/Downloads/ward shp files/nigeria_ward_boundaries/ng_wrds", layer = "ng_wrds")
ggplot(shapeward)+
  geom_sf(aes(geometry=geometry))


shapeward <- st_read(dsn = "/Users/user/Downloads/ward shp files/nigeria_ward_boundaries/ng_wrds", layer = "ng_wrds")
st_write(shapeward, 'Nigeria_Wards.shp')

shape_Katsina <- shapeward %>%
  filter(StateCode =="KT")
st_write(shape_Katsina, 'Katsina_Wards.shp')

p<-ggplot(shape_Katsina)+
  geom_sf(aes(geometry=geometry))
ggplotly(p)



shape_Delta <- shapeward %>%
  filter(StateCode =="DE")
st_write(shape_Delta, 'Delta_Wards.shp')


shape_Jigawa <- shapeward %>%
  filter(StateCode =="JI")
st_write(shape_Jigawa, 'Jigawa_Wards.shp')

shape_Yobe <- shapeward %>%
  filter(StateCode =="YO")
st_write(shape_Yobe, 'Yobe_Wards.shp')


shape_Gombe <- shapeward %>%
  filter(StateCode =="GO")
st_write(shape_Gombe, 'Gombe_Wards.shp')


shape_Taraba <- shapeward %>%
  filter(StateCode =="TA")
st_write(shape_Taraba, 'Taraba_Wards.shp')


shape_Adamawa <- shapeward %>%
  filter(StateCode =="AD")
st_write(shape_Adamawa, 'Adamawa_Wards.shp')



shape_Kano <- shapeward %>%
  filter(StateCode =="KN")
st_write(shape_Kano, 'Kano_Wards.shp')


shape_Kaduna <- shapeward %>%
  filter(StateCode =="KD")
st_write(shape_Kaduna, 'Kaduna_Wards.shp')


shape_Kwara <- shapeward %>%
  filter(StateCode =="KW")
st_write(shape_Kwara, 'Kwara_Wards.shp')


shape_Niger <- shapeward %>%
  filter(StateCode =="NI")
st_write(shape_Niger, 'Niger_Wards.shp')



shape_Ogun <- shapeward %>%
  filter(StateCode =="OG")
st_write(shape_Ogun, 'Ogun_Wards.shp')


shape_Osun <- shapeward %>%
  filter(StateCode =="OS")
st_write(shape_Osun, 'Osun_Wards.shp')

