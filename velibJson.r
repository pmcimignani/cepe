
library("jsonlite")
library(ggplot2)


json_file <- "data/velib.json"
json_data <- fromJSON(json_file)
fields <- json_data$fields

position <- fields$position

df.position<- data.frame(t(matrix(unlist(position),2)))
names(df.position)=c('lat','long')




library(ggmap)
library(RgoogleMaps)
map.OpenParis <- get_map(c(lon=2.35,lat=48.86),zoom=12,source="osm" ,maptype="roadmap") 

ggmap(map.OpenParis) +  geom_point(data =df.position, aes(x = long, y = lat))

