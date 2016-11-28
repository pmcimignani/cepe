library(RJSONIO)
library(gdata)
library(dplyr)
library(ggplot2)

#Importation des fichiers de données archivées
file201610 <- fromJSON(paste("[",paste(readLines("data/data_all_Paris.jjson_2016-11-01-1477977947.gz"),collapse=","),"]"))
file201609 <- fromJSON(paste("[",paste(readLines("data/data_all_Paris.jjson_2016-10-01-1475295963.gz"),collapse=","),"]"))
file201608 <- fromJSON(paste("[",paste(readLines("data/data_all_Paris.jjson_2016-09-01-1472703936.gz"),collapse=","),"]"))

#Définitions des fonctions pour créer les data-frames propres
returnData <- function(x,var,L){
  if(!is.null(x[[L]][[var]])){
    return(trim(x[[L]][[var]]))
  }else{
    return(NA)
  }
}

grabInfo <- function(var,L=1,file=file201610){
  print(paste("Variable", var, "Line", L, sep=" "))
  sapply(file, function(x) returnData(x,var,L))
}

any(is.na(file201610));any(is.na(file201609));any(is.na(file201608))

#Création des data-frames par mois
##Octobre 2016
datatmp <- data.frame(matrix(rep(NA,length(file201610)*8),nr=length(file201610)))
data201610 <- data.frame(NULL)
for (i in 1:length(file201610[[1]])){
  datatmp <- data.frame(sapply(1:8,grabInfo,L=i,file=file201610),stringsAsFactors = F)
  data201610 <- rbind(data201610,datatmp)
}
rm(datatmp)
any(is.na(data201610))

##September 2016
datatmp <- data.frame(matrix(rep(NA,length(file201609)*8),nr=length(file201609)))
data201609 <- data.frame(NULL)
for (i in 1:length(file201609[[1]])){
  datatmp <- data.frame(sapply(1:8,grabInfo,L=i,file=file201609),stringsAsFactors = F)
  data201609 <- rbind(data201609,datatmp)
}
rm(datatmp)
any(is.na(data201609))

##Aout 2016
datatmp <- data.frame(matrix(rep(NA,length(file201608)*8),nr=length(file201608)))
data201608 <- data.frame(NULL)
for (i in 1:length(file201608[[1]])){
  datatmp <- data.frame(sapply(1:8,grabInfo,L=i,file=file201608),stringsAsFactors = F)
  data201608 <- rbind(data201608,datatmp)
}
rm(datatmp)
any(is.na(data201608))

#Mise en forme des noms de colonnes
colnames(data201610) <- names(trim(file201610[[1]][[1]]))
colnames(data201609) <- names(trim(file201609[[1]][[1]]))
colnames(data201608) <- names(trim(file201608[[1]][[1]]))

#Aggregation dans un seul data frame et attribution des bonnes classes aux colonnes
data <- rbind.data.frame(data201610,data201609,data201608)
any(is.na(data))
data$status <- as.factor(data$status)
data$download_date <- as.POSIXct(as.numeric(data$download_date),origin="1970-01-01")
data$bike_stands <- as.numeric(data$bike_stands)
data$number <- as.integer(data$number)
data$last_update <- NULL
data$available_bike_stands <- as.numeric(data$available_bike_stands)
data$available_bikes <- as.numeric(data$available_bikes)
any(is.na(data))

#Importation des données stables pour localisation des bornes et autres infos
Stations_velib <- read.csv("stations-velib-disponibilites-en-temps-reel.csv",sep=";")
tmp <- unlist(strsplit(as.character(Stations_velib$position),","))
lat <- as.numeric(tmp[(1:length(tmp)) %% 2 == 1])
long <- as.numeric(tmp[(1:length(tmp)) %% 2 == 0])
Stations_velib$lat <- lat
Stations_velib$long <- long
rm(tmp)

Stations_velib_kept <- Stations_velib[,c("number","name","address","lat","long","banking","bonus")]
Stations_velib_kept$name <- as.character(Stations_velib_kept$name)
Stations_velib_kept$address <- as.character(Stations_velib_kept$address)

#Merge des dataframes pour regrouper toutes les infos possibles
data %>% left_join(Stations_velib_kept,by="number") -> datacomplete
any(is.na(datacomplete))
nrow(datacomplete[is.na(datacomplete$name),])
datacomplete %>% filter(!is.na(datacomplete$name)) -> datacomplete
any(is.na(datacomplete))
datacomplete %>% arrange(number,download_date) -> datacomplete
datacomplete %>% group_by(number) %>% summarise(nb=n()) -> dataperstation
table(dataperstation$nb)

#Ajout de variables calculées utiles
datacomplete %>% mutate(bikes_availability = available_bikes/bike_stands) -> datacomplete
datacomplete %>% mutate(stands_availability = available_bike_stands/bike_stands) -> datacomplete

#Graph pour voir
ggplot() +
  geom_path(data=datacomplete[(datacomplete$number==901 & as.Date(datacomplete$download_date) == as.Date("2016-09-10")),],aes(x=download_date,y=bikes_availability,col=available_bikes))

