library(ggplot2)
library(ggExtra)
library(ggthemes)
library(dbplyr)
library(readxl)
#-------------------------------------
#adding data
pop_region=read_excel("pop_region.xlsx")
World_population=read_excel("World population.xlsx")
population_Vs_years=read_excel("population Vs years.xlsx")
top_cities=read_excel("top cities.xlsx")
#-------------------------------------
#After importing Excel file
World_population<- arrange(World_population, population)
View(World_population)
#------------------------------------
#visulaize Data
d=ggplot(data = World_population, aes(World_population$Country, World_population$population)) +
geom_col(aes(fill = World_population$Country), position = "dodge") +
geom_text(aes(label = World_population$population, y = World_population$population + 0.05),
position = position_dodge(0.9),
vjust = 0)+
  scale_y_continuous("Population")+scale_x_continuous("Country")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank() 
  )+scale_fill_discrete(name = "Country Guide")
#-----------------------------------
population_Vs_years <-read.csv(file.choose(),header = T)
View(population_Vs_years)
#------------------------------------
p<-ggplot(population_Vs_years, aes(x=population_Vs_years$Year, y=population_Vs_years$TotalPopulation)) +
geom_line()+geom_point()+xlim(c(1955,2020))
+ylim(c(20000,110000))+ylab("Total Population")+
xlab("Years")+
annotate("text", x = 1970, y = 105000, 
label = "-Egypt population is equivalent to 1.31% of the total world population."
,family = "impact", fontface = "italic", colour = "darkred", size = 3)
#------------------------------------
rank <- ggplot(population_Vs_years, aes(x=population_Vs_years$Year,y=population_Vs_years$TotalPopulationRank))
rank+ geom_area(alpha=0.5)+xlim(c(1955,2020))++ylim(c(0,25))+xlab("Year")+ylab("Rank")+
  labs(title="Rank Of Egypt (2020 and historical)",
       subtitle ="-Egypt total land area is 995,450 Km2 (384,345 sq. miles)\n-43.0 % of the population is urban (44,041,052 people in 2020)"
       ,caption = "Data Source:www.worldometers.info")
#-----------------------------------
#map------------------------------
library(leaflet)
library(leaflet.extras)

basemap <- leaflet() %>%
  # add different provider tiles
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )

#-----------------------------------
#top cities
icon.fa <- makeAwesomeIcon(
  icon = "walking", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

map_1 <- basemap %>%
  addMarkers(lng = top_cities$long, lat = top_cities$lat,
             popup =as.character(top_cities$Population), label =as.character(top_cities$City))
map_1 %>% addMiniMap(toggleDisplay = TRUE) 
#-----------------------------------
#Regions 
map_2 <- basemap %>%
   addCircles(lng = pop_region$long, lat = pop_region$lat,radius =10000*100,color ="red",
popup =as.character(pop_region$Population), label =as.character(pop_region$Region))
map_2 %>% addMiniMap(toggleDisplay = TRUE) 

