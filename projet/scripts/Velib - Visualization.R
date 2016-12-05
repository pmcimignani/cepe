library(ggmap)
library(RgoogleMaps)
library(RColorBrewer)

Stations_velib %>% left_join(data201610[,c(5,19:27)],by="number") -> Stations_velib
Stations_velib %>% group_by(number) %>% do(head(.,1)) -> Stations_velib
any(is.na(Stations_velib))
Stations_velib$number[which(is.na(Stations_velib$Classe_CAH_Complete))]

#Graph pour voir
ggplot() +
  geom_path(data=data201610[(data201610$number==901 & as.Date(data201610$download_date) == as.Date("2016-10-10")),],aes(x=download_date,y=bikes_availability,col=available_bikes))

#Plan avec stations et classes
center <- c(mean(range(Stations_velib$lat)),mean(range(Stations_velib$long)))
zoom <- min(MaxZoom(range(Stations_velib$lat),range(Stations_velib$long)))
MyMap <- GetMap(center=center,zoom=zoom,destfile = "france.png")
PlotOnStaticMap(MyMap,lat=Stations_velib$lat,lon=Stations_velib$long,pch=16,cex=sqrt(Stations_velib$bike_stands)/6,col=classescomplete.open)

library(leaflet)
mypalette<-brewer.pal(max(as.numeric(as.character(Stations_velib$Classe_CAH_Complete.open)),na.rm = T),"Set1")
content <- paste(sep = "<br/>",
                 Stations_velib$name,
                 paste("Nb d'emplacements :",Stations_velib$bike_stands,sep=" ")
                 )
m <- leaflet() %>% addTiles() %>%
  setView(center[2],center[1],zoom=zoom) %>%
  addCircles(lng = Stations_velib$long,lat = Stations_velib$lat,popup = content,radius = Stations_velib$bike_stands,color = mypalette[Stations_velib$Classe_CAH_Complete.open],opacity=1)
m


