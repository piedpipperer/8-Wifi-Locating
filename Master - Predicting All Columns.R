

#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

#folder of the github root. (my project is just one folder away)
GitDirect <- "./8-Wifi-locating/"


ModelEmployed <- "rf"
approach <- "Minimal"


source(paste(GitDirect,"1 - Inicialization and Libraries.R",sep=""))
libraries_function()


#first read and binding of data.
source(paste(GitDirect,"2 - Reading CSV.R",sep=""))

Wifi1 <- retrieve_last_file(paste(getwd(),"/csv/",sep=""),"testData")  

#Wifi1$KeySample <-  1:nrow(Wifi1) 
Wifi1$TEST <- TRUE
source(paste(GitDirect,"3 - Melting&Update - WifiMelted.R",sep=""))

#melting+preprocs+removing useless waps.
TempWifiMelted1 <- WifiMelting(preprocess(
  Wifi1))


summary(TempWifiMelted1)
#needs the Rmd to be run, using those datasets

#filling up validation set!!


# summary(
# ValidationSet$BUILDING)

ValidationSet2 <- TempWifiMelted1 %>% # unique()
  tidyr::spread(WAP, SignalPow, convert = FALSE) 




for (iteration2 in c("B0", "B1","B2"))
{
  #   iteration2 <- "B0"
  
  #we have to predict the floor per building now!
  
  setwd("./8-Wifi-Locating/")
  IterFloorFit <-  readRDS(
    paste ("./models/FloorXGBoost_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet2[ValidationSet2$BUILDINGID==iteration2,]$FLOOR  <-  predict(IterFloorFit, newdata = ValidationSet2 %>%    filter(BUILDINGID == iteration2))
  
  #summary(ValidationSet2[ValidationSet2$BUILDINGID==iteration2,]$FLOOR)
  
  
  # summary(ValidationSet2$LONGITUDE)
  # always 105.
  

#lat and long trainning/validation.
FloorsToIterate <-
  ValidationSet2  %>% filter(BUILDINGID == iteration2) %>% dplyr::select(FLOOR) %>%
  unique()

for (j2 in as.vector(FloorsToIterate$FLOOR))
{
  #iteration2 <- "B0"
  #j2 <- 'F0'
  
  LongModelFit <- readRDS(
    paste ("./models/" , approach, "Long_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet2[which(ValidationSet2$BUILDINGID==iteration2 & ValidationSet2$FLOOR==j2),]$LONGITUDE  <-  predict(LongModelFit, newdata =      ValidationSet2 %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
  LatModelFit <- readRDS(
    paste ("./models/" , approach, "Lat_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet2[which(ValidationSet2$BUILDINGID==iteration2 & ValidationSet2$FLOOR==j2),]$LATITUDE  <-  predict(LatModelFit, newdata =      ValidationSet2 %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
} #floors
} #buildings




#ValidationSet2 %>% group_by(BUILDINGID,FLOOR) %>%  summarise(n = n()) 

class(Wifi1)
str(ValidationSet2)
#falta el post processing!!
#ValidationSet2$KeySample <-  1:nrow(ValidationSet2)

summary(ValidationSet2$LONGITUDE)
summary(ValidationSet2$LATITUDE)

summary(ValidationSet2$FLOOR)
summary(ValidationSet2$BUILDING)





#ValidationSet2 %>% dplyr::select(as.numeric(substr(FLOOR, 1,  4)))


levels(ValidationSet2$FLOOR) <- c(0, 1,2,3,4)
ValidationSet2$FLOOR <- as.numeric(ValidationSet2$FLOOR)-1

#ValidationSet2$GoodFloor <- as.numeric(ValidationSet2$FLOOR)-1
#ValidationSet2 %>% group_by(BUILDINGID,FLOOR,GoodFloor) %>%  summarise(n = n()) 

ValidationSet2 %>% group_by(BUILDINGID,FLOOR) %>%  summarise(n = n())
# ValidationSet2 %>% arrange(KeySample) %>% dplyr::select(LATITUDE,LONGITUDE,FLOOR,GoodFloor) %>% 
#   filter(FLOOR == "F0")


SetToCSV <- ValidationSet2 %>% arrange(KeySample) %>% dplyr::select(LATITUDE,LONGITUDE,FLOOR) 
#%>%   mutate(FLOOR = as.numeric(FLOOR))
str(SetToCSV)
SetToCSV$KeySample <- NULL
setwd("d:/dropbox/Dropbox/ubiqum/8. Wifi Locating/8-Wifi-Locating")
summary(SetToCSV)
write.csv(SetToCSV,
          paste("../csv/"  , approach, "_" ,ModelEmployed, "_ValidationSetJordi.csv",  sep = ""
               ) , row.names = FALSE 
)


# 
# Wifi4Visual <- SetToCSV %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
#                                               ,"RELATIVEPOSITION","USERID","PHONEID",
#                                               "TIMESTAMP","KeySample") )  %>% filter(value != 0) %>% unique()
# 
# colnames(Wifi4Visual)[11] <- "WAP"
# colnames(Wifi4Visual)[12] <- "SignalPow"
# Wifi4Visual$TEST <- TRUE
# summary(Wifi4Visual$LATITUDE)
# summary(Wifi4Visual$LONGITUDE)
# source("99 - WritingSpatialCSV - AllWifiAgg.R")
