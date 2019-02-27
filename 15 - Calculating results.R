
#needs the Rmd to be run, using those datasets

#filling up validation set!!

ModelEmployed <- "kknn"

for (iteration2 in c("B0", "B1","B2"))
{
  #   iteration2 <- "B0"
  
  #we have to predict the floor per building now!
  
  setwd("D:/dropbox/Dropbox/ubiqum/8. Wifi Locating/8-Wifi-Locating")
  IterFloorFit <-  readRDS(
    paste ("./models/FloorXGBoost_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet[ValidationSet$BUILDINGID==iteration2,]$FLOOR  <-  predict(IterFloorFit, newdata = ValidationSet %>%    filter(BUILDINGID == iteration2))
  
  #summary(ValidationSet[ValidationSet$BUILDINGID==iteration2,]$FLOOR)
  
  
  # summary(ValidationSet$LONGITUDE)
  # always 105.
  

#lat and long trainning/validation.
FloorsToIterate <-
  ValidationSet  %>% filter(BUILDINGID == iteration2) %>% dplyr::select(FLOOR) %>%
  unique()

for (j2 in as.vector(FloorsToIterate$FLOOR))
{
  #iteration2 <- "B0"
  #j2 <- 'F0'
  
  LongModelFit <- readRDS(
    paste ("./models/" , approach, "Long_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet[which(ValidationSet$BUILDINGID==iteration2 & ValidationSet$FLOOR==j2),]$LONGITUDE  <-  predict(LongModelFit, newdata =      ValidationSet %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
  LatModelFit <- readRDS(
    paste ("./models/" , approach, "Lat_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet[which(ValidationSet$BUILDINGID==iteration2 & ValidationSet$FLOOR==j2),]$LATITUDE  <-  predict(LatModelFit, newdata =      ValidationSet %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
} #floors
} #buildings


class(Wifi1)
str(ValidationSet)
#falta el post processing!!
#ValidationSet$KeySample <-  1:nrow(ValidationSet)

summary(ValidationSet$LONGITUDE)
summary(ValidationSet$LATITUDE)


summary(as.vector(Wifi1 %>% inner_join(ValidationSet %>% dplyr::select(BUILDINGID,KeySample)
                                       , by = "KeySample") %>% 
                    dplyr::select( BUILDINGID = BUILDINGID.y)# %>% rename(BUILDINGID = BUILDINGID.BUILDINGID)
))


# 
# SetToCSV <- Wifi0
# cambiar estrategia , guardar validation set y arreange por keysample
# 
# #tail(SetToCSV)
# 
# #SetToCSV$BUILDINGID <- SetToCSV$BUILDINGID.BUILDINGID
# #SetToCSV$BUILDINGID.BUILDINGID <- NULL
# 
# 
# SetToCSV$FLOOR <- as.vector(Wifi1 %>% inner_join(ValidationSet %>% dplyr::select(FLOOR,KeySample)
#                                                  , by = "KeySample") %>% 
#                               dplyr::select(FLOOR = FLOOR.y)) 
# 
# SetToCSV$LATITUDE <- as.vector(Wifi1 %>% inner_join(ValidationSet %>% dplyr::select(LATITUDE,KeySample)
#                                                     , by = "KeySample") %>% 
#                                  dplyr::select(LATITUDE = LATITUDE.y) )
# 
# SetToCSV$LONGITUDE <- as.vector(Wifi1 %>% inner_join(ValidationSet %>% dplyr::select(LONGITUDE,KeySample)
#                                                      , by = "KeySample") %>% 
#                                   dplyr::select(LONGITUDE = LONGITUDE.y )
# )

SetToCSV <- ValidationSet %>% arrange(KeySample) %>% select (BUILDINGID,FLOOR,LATITUDE,LONGITUDE)
str(SetToCSV)
SetToCSV$KeySample <- NULL
setwd("d:/dropbox/Dropbox/ubiqum/8. Wifi Locating/8-Wifi-Locating")
write.csv(SetToCSV,
          paste("../csv/"  , approach, "_" ,ModelEmployed, "_ValidationSetJordi.csv",  sep = ""
                )
)



Wifi4Visual <- SetToCSV %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                              ,"RELATIVEPOSITION","USERID","PHONEID",
                                              "TIMESTAMP","KeySample") )  %>% filter(value != 0) %>% unique()

colnames(Wifi4Visual)[11] <- "WAP"
colnames(Wifi4Visual)[12] <- "SignalPow"
Wifi4Visual$TEST <- TRUE
summary(Wifi4Visual$LATITUDE)
summary(Wifi4Visual$LONGITUDE)
source("99 - WritingSpatialCSV - AllWifiAgg.R")
