

#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))


ModelEmployed <- "knn"
approach <- "Minimal"


#folder of the github root. (my project is just one folder away)
GitDirect <- "./8-Wifi-locating/"


source(paste(GitDirect,"1 - Inicialization and Libraries.R",sep=""))
libraries_function()


#first read and binding of data.
source(paste(GitDirect,"2 - Reading CSV.R",sep=""))

Wifi1 <- preprocess(retrieve_last_file(paste(getwd(),"/csv/",sep=""),"testData") ) 

Wifi1$KeySample <-  1:nrow(Wifi1) 
#Wifi1$TEST <- TRUE
source(paste(GitDirect,"3 - Melting&Update - WifiMelted.R",sep=""))

#melting+preprocs+removing useless waps.
TempWifiMelted1 <- WifiMelting(
  Wifi1)


summary(TempWifiMelted1)
#needs the Rmd to be run, using those datasets

#filling up validation set!!


# summary(
# ValidationSet$BUILDING)

ValidationSet2 <- Wifi1 #%>% 
 # tidyr::spread(WAP, SignalPow, convert = FALSE) 


ValidationSet2$FLOOR <- "F4"
ValidationSet2$FLOOR <- factor(ValidationSet2$FLOOR)
levels(ValidationSet2$FLOOR) <- c("F0", "F1","F2","F3","F4")

XGBoostBuildFitt <- readRDS(paste(GitDirect,"./models/BuildXGBoost.rds",sep=""))

ValidationSet2$BUILDINGID <-  predict(XGBoostBuildFitt, newdata = ValidationSet2)




for (iteration2 in c("B0", "B1","B2"))
{
  #   iteration2 <- "B0"
  
  #we have to predict the floor per building now!
  

  IterFloorFit <-  readRDS(
    paste (GitDirect,"./models/FloorXGBoost_" , iteration2 , ".rds", sep = "")
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
  #j2 <- 'F1'
  
  LongModelFit <- readRDS(
    paste (GitDirect,"./models/" , approach, "Long_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet2[which(ValidationSet2$BUILDINGID==iteration2 & ValidationSet2$FLOOR==j2),]$LONGITUDE  <-  predict(LongModelFit, newdata =      ValidationSet2 %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
  LatModelFit <- readRDS(
    paste (GitDirect,"./models/" , approach, "Lat_" , ModelEmployed , "_" , j2 , "_" , iteration2 , ".rds", sep = "")
  )
  ValidationSet2[which(ValidationSet2$BUILDINGID==iteration2 & ValidationSet2$FLOOR==j2),]$LATITUDE  <-  predict(LatModelFit, newdata =      ValidationSet2 %>%    filter(BUILDINGID == iteration2) %>%    filter(FLOOR == j2)) #3900
  
} #floors
} #buildings




#ValidationSet2 %>% group_by(BUILDINGID,FLOOR) %>%  summarise(n = n()) 

# summary(ValidationSet2$LONGITUDE)
# summary(ValidationSet2$LATITUDE)
# 
# summary(ValidationSet2$FLOOR)
# summary(ValidationSet2$BUILDING)


#ValidationSet2 %>% dplyr::select(as.numeric(substr(FLOOR, 1,  4)))



#post processing of the modeling:
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

summary(SetToCSV)
write.csv(SetToCSV,
          paste("./csv/"  , approach, "_" ,ModelEmployed, "_ValidationSetPiedPipperer.csv",  sep = ""
               ) , row.names = FALSE 
)


# 
# Wifi4Visual <- TempWifiMelted1  %>% filter(value != 0) %>% unique()
# 
# colnames(Wifi4Visual)[11] <- "WAP"
# colnames(Wifi4Visual)[12] <- "SignalPow"
# Wifi4Visual$TEST <- TRUE
# summary(Wifi4Visual$LATITUDE)
# summary(Wifi4Visual$LONGITUDE)
# source("99 - WritingSpatialCSV - AllWifiAgg.R")
