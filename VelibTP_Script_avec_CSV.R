#Importation fichier OpenData Velib
Stations_velib <- read.csv("stations-velib-disponibilites-en-temps-reel.csv",sep=";")

par(mfrow=c(1,2))
hist(Stations_velib$bike_stands,xlab="Taille de station",main=NULL)
plot(density(Stations_velib$bike_stands), main="Density of Station Size")
polygon(density(Stations_velib$bike_stands), col="red", border="blue")

#Split des coordonnées
tmp <- unlist(strsplit(as.character(Stations_velib$position),","))
lat <- as.numeric(tmp[(1:length(tmp)) %% 2 == 1])
long <- as.numeric(tmp[(1:length(tmp)) %% 2 == 0])

#Ajout de la latitude et de la longitude dans le dataset
Stations_velib$lat <- lat
Stations_velib$long <- long
par(mfrow=c(1,1))
plot(Stations_velib$lat~Stations_velib$long)

library(ggplot2)
library(ggmap,lib.loc = "C:\\Users/Cepe-S1-07/rlib/")
library(RgoogleMaps,lib.loc = "C:\\Users/Cepe-S1-07/rlib/")

center <- c(mean(range(Stations_velib$lat)),mean(range(Stations_velib$long)))
zoom <- min(MaxZoom(range(Stations_velib$lat),range(Stations_velib$long)))
MyMap <- GetMap(center=center,zoom=zoom,destfile = "france.png")
PlotOnStaticMap(MyMap,lat=Stations_velib$lat,lon=Stations_velib$long,pch=16,cex=sqrt(Stations_velib$bike_stands)/6,col=Stations_velib$bonus)

library(leaflet,lib.loc = "C:\\Users/Cepe-S1-07/rlib/")

m <- leaflet() %>% addTiles() %>%
  setView(center[2],center[1],zoom=zoom) %>%
  addCircleMarkers(lng = Stations_velib$long,lat = Stations_velib$lat,radius=Stations_velib$bike_stands/10)
m
