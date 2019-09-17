


#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))


#folder config (to work on any computer, all files are in github folder)
library(rstudioapi)
current_path <- #"D:/dropbox/Dropbox/ubiqum/8. Wifi Locating/8-Wifi-locating/PEPE.R" 
  rstudioapi::getActiveDocumentContext()$path 

setwd(dirname(current_path))

GitDirect <- "./"


source(paste(GitDirect,"1 - Inicialization and Libraries.R",sep=""))
libraries_function()

#first read and binding of data.
source(paste(GitDirect,"2 - Reading CSV.R",sep=""))
#summary(Wifi0)
TrainWifi0 <- retrieve_last_file(paste(paste(getwd(),GitDirect,sep=""),"/csv/",sep=""),"trainingData")
TestWifi0 <- retrieve_last_file(paste(paste(getwd(),GitDirect,sep=""),"/csv/",sep=""),"testData")

TestWifi0$TEST <- TRUE 
TrainWifi0$TEST <- FALSE 

#1st preprocessing + binding of both datasets
Wifi0 <- preprocess(
  rbind(TrainWifi0,TestWifi0)
)

source(paste(GitDirect,"3 - Melting&Update - WifiMelted.R",sep=""))

#melting+preprocs+removing useless waps.
WifiMelted <- RemoveBotheringWAPs(   #ONLY executed in trainnning, not in predicting
  WifiMelting(Wifi0)
)


source(paste(GitDirect,"4 - SelectUniqueLocs.R",sep=""))



#train the different floors based on the variable iteration.

#### Trainnnig Floor ####
for (iteration in c("B0"
                    ,
                    "B1"
                    ,
                    "B2"
                    ))
{  #iteration <- "B1"
  


TrainFloorAlls <-
  RemoveBotheringWAPs(WifiMelted %>% filter(BUILDINGID %in% c(iteration)) %>%
                        dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample)
  )  %>%  tidyr::spread(WAP, SignalPow, convert = FALSE)

#str(TrainFloorAlls)

#source(paste(GitDirect,"8 - Trainning Floor.R",sep=""))


IterateDF2 <- TakeNumberUniqueLocations(TrainFloorAlls,18 #number of iterations to take all unique locations
                                        )
TestIterate <- TrainFloorAlls %>%
  subset(!KeySample %in% IterateDF2$KeySample)

IterateDF3 <- IterateDF2 %>% ungroup() %>% select (-LONGITUDE,-LATITUDE,-KeySample,-BUILDINGID)

#IterateDF3 %>% group_by(FLOOR) %>%    summarise(n = n()) %>% arrange(n)

IterateDF3$FLOOR <- factor(IterateDF3$FLOOR)


FloorControl <- caret::trainControl(method="repeatedcv"
                             ,classProbs = TRUE
                             #,savePredictions = TRUE
                             , number=5#, repeats=3
)

set.seed(123)
XGBoostFloorFit <- train(FLOOR~.,
                         data = IterateDF3,
                         method = "xgbTree",
                         tunelength = 5,
                         trControl = FloorControl#,tuneGrid=parametersGrid
)

saveRDS(XGBoostFloorFit,
paste (GitDirect,"./models/FloorXGBoost_" , iteration , ".rds", sep = "")
)

}







#after trainned, see results:

XGBoostFloorFit <- readRDS(paste ("./models/FloorXGBoost_" , iteration , ".rds", sep = ""))

#això ho hem de fer abans d'asignar null a les variables 

#3k1 
#varImp(XGBoostFloorFit)
#Models <- list.files(path = "./models/", pattern = "rds")

GenericTrClasses <- predict(XGBoostFloorFit, newdata = TrainWifiFloor)
Matrix <-   confusionMatrix(GenericTrClasses, TrainWifiFloor$FLOOR)

GenericTestClasses <- predict(XGBoostFloorFit, newdata = TestIterate)
Matrix <-   confusionMatrix(GenericTestClasses, TestIterate$FLOOR)
