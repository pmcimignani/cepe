
library("jsonlite")
library(ggplot2)


json_file <- "data/velib.json"
json_data <- fromJSON(json_file)
fields <- json_data$fields
bikeStands <- fields$bike_stands
position <- fields$position
length(position)

df.position<- as.data.frame(position)
dim(df.position)

latitudes <- unlist(df.position[1,])
longitudes <- unlist(df.position[2,])



library(ggmap)
library(RgoogleMaps)
map.OpenParis <- get_map(c(lon=2.35,lat=48.86),zoom=12,source="osm" ,maptype="roadmap") 

ggmap(map.OpenParis) +  geom_point(data = json_data, aes(x = longitudes, y = latitudes))

