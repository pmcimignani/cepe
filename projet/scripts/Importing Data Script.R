library(jsonlite)
library(ndjson)
library(dplyr)
library(ggplot2)
library(Amelia)

json_file_201610 <- as.data.frame(ndjson::stream_in("data/data_all_Paris.jjson_2016-11-01-1477977947.gz"))
json_file_201609 <- as.data.frame(ndjson::stream_in("data/data_all_Paris.jjson_2016-10-01-1475295963.gz"))
json_file_201608 <- as.data.frame(ndjson::stream_in("data/data_all_Paris.jjson_2016-09-01-1472703936.gz"))
str(json_file_201610)
str(json_file_201609)
str(json_file_201608)

#"number": identifiant de la station
#"bike_stands": nombre de bornes
#"available_bike_stands": nombre de bornes disponnibles
#"available_bikes": nombre de vélos disponnibles
#"download_date": date de téléchargement des données (TimeStamp Unix)
#"contract_name": ville étudiée
#"status": statut de la station étudiée (OPEN or CLOSED)

#########################
#Réorganisation des data#
#########################
any(is.na(json_file_201610));any(is.na(json_file_201609));any(is.na(json_file_201608))
str(json_file_201610[9700:9792])
#Les 16 dernières colonnes du fichier d'Octobre sont des NA que nous allons retirer.
json_file_201610 <- json_file_201610[,-(9777:9792)]

str(json_file_201609[9700:9776])
#Les 8 dernières colonnes du fichier de Septembre sont des NA que nous allons retirer.
json_file_201609 <- json_file_201609[,-(9769:9776)]

#Choix des colonnes utiles
available_bike_stands_fields <- seq(1,length(json_file_201610)-7,by=8)
available_bikes_fields <- seq(2,length(json_file_201610)-6,by=8)
bike_stands_fields <- seq(3,length(json_file_201610)-5,by=8)
download_date_fields <- seq(5,length(json_file_201610)-3,by=8)
station_number <- seq(7,length(json_file_201610)-1,by=8)
status_fields <- seq(8,length(json_file_201610),by=8)

#Mise en forme de la date
json_file_201610[,download_date_fields] <- lapply(json_file_201610[,download_date_fields],as.POSIXct,origin="1970-01-01")

#Création du dataframe pour les emplacements de Velib dispos
available_bike_stands_201610 <- data.frame(cbind(json_file_201610$`0.download_date`,json_file_201610[,available_bike_stands_fields]))
colnames(available_bike_stands_201610) <- c("Time",json_file_201610[1,station_number])

#Création du dataframe pour les Velib dispos
available_bikes_201610 <- data.frame(cbind(json_file_201610$`0.download_date`,json_file_201610[,available_bikes_fields]))
colnames(available_bikes_201610) <- c("Time",json_file_201610[1,station_number])

#Création du dataframe pour le statut des stations Velib
station_status_201610 <- data.frame(cbind(json_file_201610$`0.download_date`,json_file_201610[,status_fields]))
colnames(station_status_201610) <- c("Time",json_file_201610[1,station_number])
station_status_201610[,-1] <- lapply(station_status_201610[,-1],as.factor)
for (i in 2:length(station_status_201610)) {levels(station_status_201610[,i]) <- c("OPEN","CLOSED")}

#Un exemple de visu sur une station
ggplot() + 
  geom_path(data=available_bike_stands_201610[as.Date(available_bike_stands_201610$Time) == as.Date("2016-09-06"),],mapping=aes(x=Time,y=`21206`),lineend = "butt",linejoin = "round",linemitre = 1,col="red") +
  geom_path(data=available_bikes_201610[as.Date(available_bikes_201610$Time) == as.Date("2016-09-06"),],mapping=aes(x=Time,y=`21206`),lineend = "butt",linejoin = "round",linemitre = 1,col="blue")

#Check du nb maximum de Velib par station
maxVelib <- sapply(available_bike_stands_201610[,-1]+available_bikes_201610[,-1],max)
max_bike_stands <- sapply(json_file_201610[,bike_stands_fields],max)
sum(maxVelib != max_bike_stands)
#Apparamment, il y a 227 stations pour lesquelles le nombre max de Velib comme la somme de velo + bornes dispos n'est pas égal au nb de "bike_stands" dans le fichier initial.
# A creuser...

