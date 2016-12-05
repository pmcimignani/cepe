#Travail préalable sur les données pour travailler seulement sur un échantillon
#Ne pas tenir car déjà dans script importation et traitement de données
data201610$status <- as.factor(data201610$status)
data201610$download_date <- as.POSIXct(as.numeric(data201610$download_date),origin="1970-01-01")
data201610$bike_stands <- as.numeric(data201610$bike_stands)
data201610$number <- as.integer(data201610$number)
data201610$last_update <- NULL
data201610$available_bike_stands <- as.numeric(data201610$available_bike_stands)
data201610$available_bikes <- as.numeric(data201610$available_bikes)
any(is.na(data201610))
data201610 %>% left_join(Stations_velib_kept,by="number") -> data201610
any(is.na(data201610))
nrow(data201610[is.na(data201610$name),])
data201610 %>% filter(!is.na(data201610$name)) -> data201610
any(is.na(data201610))
data201610 %>% arrange(number,download_date) -> data201610

data201610 %>% group_by(number) %>% summarise(nb=n()) -> dataperstation
table(dataperstation$nb)
as.data.frame(dataperstation)[dataperstation$nb<2223,]
tail(data201610[data201610$number=='44102',])
nrow(data201610[data201610$number=='44101',])
head(data201610[data201610$number=='42003',])
tail(data201610[data201610$number=='16115',])
head(data201610[data201610$number=='15063',])
stationstoexclude <- c("15063","16115","42003","44101","44102")
data201610 <- data201610[!(data201610$number %in% stationstoexclude),]
data201610 %>% group_by(number) %>% summarise(nb=n()) -> dataperstation
table(dataperstation$nb)

#Ajout de variables pour classification
data201610$day <- mday(data201610$download_date)
data201610$wday <- lubridate::wday(data201610$download_date,label=T,abbr = F)
data201610$time <- format(as.POSIXct(data201610$download_date,format="%Y-%m-%d %H:%M:%S"),format="%H:%M")
data201610$bikes_availability <- data201610$available_bikes/data201610$bike_stands
data201610$stands_availability <- data201610$available_bike_stands/data201610$bike_stands

##########################################################################################

#Définition d'un dataframe pour classification selon bikes_availability par jour
data201610 %>% 
  group_by(number,time) %>%
  summarise(m=mean(bikes_availability)) -> dataclassif
length(unique(dataclassif$number))

##Classification over the whole week via CAH
dataclassifweekly <- data.frame(matrix(rep(NA,length(unique(dataclassif$number))*73),nr=length(unique(dataclassif$number))))
colnames(dataclassifweekly) <- c("number",dataclassif$time[1:72])
dataclassifweekly$number <- unique(dataclassif$number)
for (i in 1:length(unique(dataclassif$number))){
  dataclassifweekly[i,2:73] <- t(as.numeric(dataclassif$m[dataclassif$number==dataclassifweekly[i,1]]))
}
any(is.na(dataclassifweekly))
summary(dataclassifweekly)
max(dataclassifweekly[,-1])
#dataclassifweekly %>% left_join(Stations_velib[,c(1,5:7)],by="number") -> dataclassifweekly

###Several methods used
hc.complete=hclust(dist(dataclassifweekly,method="manhattan"), method="complete")
hc.average=hclust(dist(dataclassifweekly,method="manhattan"), method="average")
hc.single=hclust(dist(dataclassifweekly,method="manhattan"), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
par(mfrow=c(1,1))
plot(rev(hc.complete$height),type="h",ylab="hauteurs",xlim=c(0,20))
classescomplete <- cutree(hc.complete,k=4)
plot(rev(hc.average$height),type="h",ylab="hauteurs",xlim=c(0,20))
classesaverage <- cutree(hc.average,k=4)
plot(rev(hc.single$height),type="h",ylab="hauteurs",xlim=c(0,20))
classessingle <- cutree(hc.single,k=3)
dataclassifweekly <- cbind.data.frame(dataclassifweekly,as.factor(classescomplete),as.factor(classesaverage),as.factor(classessingle))
colnames(dataclassifweekly)[74:76] <- c("Classe_CAH_Complete","Classe_CAH_Average","Classe_CAH_Single")

###Rajout des classes dans le dataframe global de départ
data201610 %>% left_join(dataclassifweekly[,c(1,74:76)],by="number") -> data201610
dtdata201610 <- as.data.table(data201610)
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Complete","time")] -> dataforplotCAHcomplete
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Average","time")] -> dataforplotCAHaverage
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Single","time")] -> dataforplotCAHsingle

###Plotting des classes
ggplot(data=dataforplotCAHcomplete,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Complete)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the week") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))
  
ggplot(data=dataforplotCAHaverage,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Average)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the week") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

ggplot(data=dataforplotCAHsingle,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Single)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the week") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

####################################################################################

#Définition d'un dataframe pour classification selon bikes_availability par jour ouvré
data201610 %>% 
  filter(wday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) %>%
  group_by(number,time) %>%
  summarise(m=mean(bikes_availability)) -> dataclassifopen

##Classification over the working days via CAH
dataclassifweeklyopen <- data.frame(matrix(rep(NA,length(unique(dataclassifopen$number))*73),nr=length(unique(dataclassifopen$number))))
colnames(dataclassifweeklyopen) <- c("number",dataclassifopen$time[1:72])
dataclassifweeklyopen$number <- unique(dataclassifopen$number)
for (i in 1:length(unique(dataclassifopen$number))){
  dataclassifweeklyopen[i,2:73] <- t(as.numeric(dataclassifopen$m[dataclassifopen$number==dataclassifweeklyopen[i,1]]))
}
any(is.na(dataclassifweeklyopen))
summary(dataclassifweeklyopen)
max(dataclassifweeklyopen[,-1])
#dataclassifweeklyopen %>% left_join(Stations_velib[,c(1,5:7)],by="number") -> dataclassifweeklyopen

###Several methods used
hc.complete.open=hclust(dist(dataclassifweeklyopen,method="manhattan"), method="complete")
hc.average.open=hclust(dist(dataclassifweeklyopen,method="manhattan"), method="average")
hc.single.open=hclust(dist(dataclassifweeklyopen,method="manhattan"), method="single")
par(mfrow=c(1,3))
plot(hc.complete.open,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average.open, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single.open, main="Single Linkage", xlab="", sub="", cex=.9)
par(mfrow=c(1,1))
plot(rev(hc.complete.open$height),type="h",ylab="hauteurs",xlim=c(0,20))
classescomplete.open <- cutree(hc.complete.open,k=4)
plot(rev(hc.average.open$height),type="h",ylab="hauteurs",xlim=c(0,20))
classesaverage.open <- cutree(hc.average.open,k=4)
plot(rev(hc.single.open$height),type="h",ylab="hauteurs",xlim=c(0,20))
classessingle.open <- cutree(hc.single.open,k=3)
dataclassifweeklyopen <- cbind.data.frame(dataclassifweeklyopen,as.factor(classescomplete.open),as.factor(classesaverage.open),as.factor(classessingle.open))
colnames(dataclassifweeklyopen)[74:76] <- c("Classe_CAH_Complete.open","Classe_CAH_Average.open","Classe_CAH_Single.open")

###Rajout des classes dans le dataframe global de départ
data201610 %>% left_join(dataclassifweeklyopen[,c(1,74:76)],by="number") -> data201610
dtdata201610 <- as.data.table(data201610)
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Complete.open","time")] -> dataforplotCAHcompleteopen
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Average.open","time")] -> dataforplotCAHaverageopen
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Single.open","time")] -> dataforplotCAHsingleopen

###Plotting des classes
ggplot(data=dataforplotCAHcompleteopen,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Complete.open)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the working days") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

ggplot(data=dataforplotCAHaverageopen,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Average.open)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the working days") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

ggplot(data=dataforplotCAHsingleopen,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Single.open)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the working days") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

###################################################################

#Définition d'un dataframe pour classification selon bikes_availability par jour ouvré
data201610 %>% 
  filter(wday %in% c("Saturday","Sunday")) %>%
  group_by(number,time) %>%
  summarise(m=mean(bikes_availability)) -> dataclassifWEonly

##Classification over the working days via CAH
dataclassifWE <- data.frame(matrix(rep(NA,length(unique(dataclassifWEonly$number))*73),nr=length(unique(dataclassifWEonly$number))))
colnames(dataclassifWE) <- c("number",dataclassifWEonly$time[1:72])
dataclassifWE$number <- unique(dataclassifWEonly$number)
for (i in 1:length(unique(dataclassifWEonly$number))){
  dataclassifWE[i,2:73] <- t(as.numeric(dataclassifWEonly$m[dataclassifWEonly$number==dataclassifWE[i,1]]))
}
any(is.na(dataclassifWE))
summary(dataclassifWE)
max(dataclassifWE[,-1])
#dataclassifWE %>% left_join(Stations_velib[,c(1,5:7)],by="number") -> dataclassifWE

###Several methods used
hc.complete.WE=hclust(dist(dataclassifWE,method="manhattan"), method="complete")
hc.average.WE=hclust(dist(dataclassifWE,method="manhattan"), method="average")
hc.single.WE=hclust(dist(dataclassifWE,method="manhattan"), method="single")
par(mfrow=c(1,3))
plot(hc.complete.WE,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average.WE, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single.WE, main="Single Linkage", xlab="", sub="", cex=.9)
par(mfrow=c(1,1))
plot(rev(hc.complete.WE$height),type="h",ylab="hauteurs",xlim=c(0,20))
classescomplete.WE <- cutree(hc.complete.WE,k=4)
plot(rev(hc.average.WE$height),type="h",ylab="hauteurs",xlim=c(0,20))
classesaverage.WE <- cutree(hc.average.WE,k=4)
plot(rev(hc.single.WE$height),type="h",ylab="hauteurs",xlim=c(0,20))
classessingle.WE <- cutree(hc.single.WE,k=3)
dataclassifWE <- cbind.data.frame(dataclassifWE,as.factor(classescomplete.WE),as.factor(classesaverage.WE),as.factor(classessingle.WE))
colnames(dataclassifWE)[74:76] <- c("Classe_CAH_Complete.WE","Classe_CAH_Average.WE","Classe_CAH_Single.WE")

###Rajout des classes dans le dataframe global de départ
data201610 %>% left_join(dataclassifWE[,c(1,74:76)],by="number") -> data201610
dtdata201610 <- as.data.table(data201610)
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Complete.WE","time")] -> dataforplotCAHcompleteWE
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Average.WE","time")] -> dataforplotCAHaverageWE
dtdata201610[,.(m=mean(bikes_availability)),by=c("Classe_CAH_Single.WE","time")] -> dataforplotCAHsingleWE

###Plotting des classes
ggplot(data=dataforplotCAHcompleteWE,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Complete.WE)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the WE") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

ggplot(data=dataforplotCAHaverageWE,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Average.WE)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the WE") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

ggplot(data=dataforplotCAHsingleWE,aes(x=time,y=m)) +
  geom_point(aes(col=Classe_CAH_Single.WE)) +
  labs(x = "Hours of the day", y = "Average Bike Availability over the WE") +
  theme_minimal() +
  scale_x_discrete(breaks=c("02:00","04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))