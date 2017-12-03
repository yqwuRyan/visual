library(rgdal)
library(shiny)
library(leaflet)
library(RColorBrewer)
#library(scales)
#library(lattice)
library(tidyverse)
library(sp)
### library(sf)  # Try this to replace 'sp'
library(data.table)
library(tidygraph)
library(DT)          # Dynamic-using Javascript Library  
library(raster)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(treemap)
library(d3treeR)
library(igraph)
library(plotly)

#setwd("Y:/Local/Testing/version1130")
#setwd("C:/Users/wuyuy/Dropbox/Visual Project/Shiny Code/version1201")

# Import Data
flight <- fread("data/flight.csv")
location <- fread("data/location.csv")
airline <- fread("data/airline.csv")

location2 <- location
airline2<- airline

flight2 <- merge(flight,location,by.x='Source',by.y='IATA')[,c(1:3,7,8)]
flight2 <- merge(flight2,location,by.x='Target',by.y='IATA')[,c(1:5,9:10)]
colnames(flight2)[4:7]=c('Scountry','Sregion','Tcountry','Tregion')
flight2 <- merge(flight2,location,by.x='Source',by.y='IATA')[,c(1:7,10)]
flight2 <- merge(flight2,location,by.x='Target',by.y='IATA')[,c(1:8,11)]
flight13 <- fread("data/flight2013.csv") %>% mutate("Year"="2013")
colnames(flight13)[4]="Weight"
flight17 <- fread("data/flight2017.csv") %>% mutate("Year"="2017")
tflight<-rbind(flight13,flight17)[,2:11]
colnames(flight2)[8:9]=c('Sairport','Tairport')

a=airline %>% group_by(Dep,Arr,Airline) %>% summarise(value=n())
flight3 <- merge(flight2,a,by.x=c("Target","Source"),by.y=c("Arr","Dep"))[,c(1:2,4:11)]
world <- readOGR("data/shapefile/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp",
                 layer = "TM_WORLD_BORDERS_SIMPL-0.3", GDAL1_integer64_policy = TRUE)
BR_country <- read.csv("data/shapefile/B&R country code.csv")

selected <- subset(world, world$ISO2 %in% BR_country$ISO2.Code)
selected1 <- merge(selected, BR_country, by.x="ISO3",by.y="ISO3.Code")
selected2 <- raster::aggregate(selected1,"Region")
country_list=selected@data$NAME[c(1:11,13:length(selected@data$NAME))]
region_list=location$Region
airport_list=location$Airport

# Calculate Centrality 
china_flight <- tbl_graph(nodes = location, edges = flight, directed = FALSE)
m<-china_flight %>%  
  mutate(betweenness_centrality = centrality_betweenness()) %>%
  mutate(closeness_centrality = centrality_closeness()) %>% 
  mutate(degree_centrality = centrality_degree())
location<-as.data.frame(m)

# Define data frame
df<-data.frame(flight2)
colnames(df)[1:2]=c("to","from")

add<-function(x,y,b){
  if (x==b) return (x)
  else return (y)
}

add2<-function(x,y,b){
  if (x==b) return (y)
  else return (x)
}


region_analysis = tflight %>% 
  group_by("Region"=Tregion,Year) %>% 
  summarise("No.of Air Routes"=n(),"No.of Flights"=sum(Weight))

country_analysis1 = tflight %>% 
  group_by("Country"=Tcountry,Year) %>% 
  summarise("No.of Flights"=sum(Weight)) %>%
  spread(key=Year,value=`No.of Flights`,fill=0) %>%
  mutate("Growth"=`2017`-`2013`)

country_analysis2 = tflight %>% 
  group_by("Country"=Tcountry,Year) %>% 
  summarise("No.of Air Routes"=n()) %>%
  spread(key=Year,value=`No.of Air Routes`,fill=0) %>%
  mutate("Growth"=`2017`-`2013`)
  
airport_analysis1 = tflight %>%
  subset(Year=="2017") %>%
  group_by("Airport"=Sairport) %>% 
  summarise("No.of Air Routes"=n()) %>%
  arrange(desc(`No.of Air Routes`)) %>%
  slice(1:10) %>%
  arrange(`No.of Air Routes`)
  
airport_analysis2 = tflight %>%
  subset(Year=="2017") %>%
  group_by("Airport"=Sairport) %>% 
  summarise("No.of Flights"=sum(Weight)) %>%
  arrange(desc(`No.of Flights`)) %>%
  slice(1:10) %>%
  arrange(`No.of Flights`)
