#Wifi Data Explorations
#Jordi Rojo
#12.02.2019


#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

#folder of the github root. (my project is just one folder away)
GitDirect <- "./8-Wifi-locating/"


source(paste(GitDirect,"1 - Inicialization and Libraries.R",sep=""))
libraries_function()

#first read and binding of data.
source(paste(GitDirect,"2 - Reading CSV.R",sep=""))
#summary(Wifi0)
TrainWifi0 <- retrieve_last_file(paste(paste(getwd(),GitDirect,sep=""),"/csv/",sep=""),"trainingData")
TestWifi0 <- retrieve_last_file(paste(paste(getwd(),GitDirect,sep=""),"/csv/",sep=""),"validationData")

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


#writing csv with coordinates 4 viz:
Wifi4Visual <- WifiMelted %>% filter(SignalPow != 0) %>% unique()
source("99 - WritingSpatialCSV - AllWifiAgg.R")



# Trainning of the LongLat models
#approach <- "Minimal"
approach <- ""   #1st approach.
#approach <- "OnlyTrain"

source(paste(GitDirect,"4 - SelectUniqueLocs.R",sep=""))


#### trainning building #### 
# TrainWifiAllLocs <- TakeNumberUniqueLocations(WifiUnMelt,20)
# TestWifi <- WifiUnMelt %>%
#   subset(!KeySample %in% TrainWifiAllLocs$KeySample) 

#source(paste(GitDirect,"6 - Trainnning Building.R",sep=""))



#### Trainnnig Floor ####  executed in another Master File
#iteration <- "B0"
# TrainFloorAlls <- 
#   RemoveBotheringWAPs(WifiMelted2 %>% filter(BUILDINGID %in% c(iteration)) %>% 
#                         dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
#   )  %>%  tidyr::spread(WAP, SignalPow, convert = FALSE)

#source(paste(GitDirect,"8 - Trainning Floor.R",sep=""))

#train the different floors based on the variable iteration.





# getting a training set with the relevant waps. (for minimal approach)
BadWapss <- WifiMelted   %>% dplyr::select(WAP) 
if (approach == "Minimal") {
  BadWapss <- retrieve_last_file(paste(getwd(),"/csv/",sep=""),"BadWaps")
  #previously stored file with the Waps not used in ValidationSet.
  
}


WifiMelt2 <- WifiMelted  %>%
  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID, WAP, SignalPow, KeySample, TEST) %>% 
  subset(!WAP %in% BadWapss$WAP) 


WifiUnMelt <- WifiMelt2   %>% 
  tidyr::spread(WAP, SignalPow, convert = FALSE) 

if (approach == "OnlyTrain")
{ 
  WifiMelted2 <- WifiMelted %>% filter(TEST == FALSE)
} else {
  WifiMelted2 <- WifiMelted
}






#loop over buildings
  for (iteration in c("B0"
                      ,
                      "B1"
                      ,
                      "B2"
                      )) 
{ 
  #iteration <- "B2"
  
  #subsetting (and removing useless waps for that building)
  TrainFloorAlls <- 
    RemoveBotheringWAPs(WifiMelted2 %>% filter(BUILDINGID %in% c(iteration)) %>% 
                    dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
    )  %>%  tidyr::spread(WAP, SignalPow, convert = FALSE)
  
  #floors can be different in the building
  TrainFloorAlls$FLOOR <- factor(TrainFloorAlls$FLOOR)
  

  #lat and long trainning/validation.
  FloorsToIterate <- 
    TrainFloorAlls  %>% dplyr::select(FLOOR) %>% #group_by(FLOOR) %>% ungroup() %>%
   unique()
  
  #source("8 - Trainning Floor.R")
  #as.vector(FloorsToIterate$FLOOR)


  for (j in  as.vector(FloorsToIterate$FLOOR) #c("F0","F1")
       )
  {
    #iteration <- "B0"
    #j <- 'F2'
    
    #subsetting+remove useless waps for that floor-building
    TrainLong <- 
      RemoveBotheringWAPs(WifiMelted2 %>% filter(BUILDINGID %in% c(iteration)) %>% filter(FLOOR %in% c(j)) %>% 
                            dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
      )  %>% tidyr::spread(WAP, SignalPow, convert = FALSE)
   
    TrainLat <- TrainLong
    
    TrainNN <- TrainLong
    TrainNN$BUILDINGID <- NULL
    TrainNN$KeySample <- NULL
    TrainNN$FLOOR <- NULL
    
    #TrainLatLong %>% select(FLOOR,BUILDINGID) %>% group_by(FLOOR,BUILDINGID)  %>% unique()
    TrainLong$BUILDINGID <- NULL
    TrainLong$KeySample <- NULL
    TrainLong$FLOOR <- NULL
    TrainLong$LATITUDE <- NULL
    
    
    #TrainLatLong %>% select(FLOOR,BUILDINGID) %>% group_by(FLOOR,BUILDINGID)  %>% unique()
    TrainLat$BUILDINGID <- NULL
    TrainLat$KeySample <- NULL
    TrainLat$FLOOR <- NULL
    TrainLat$LONGITUDE <- NULL
    
    Models <- c(##"vbmpRadial" #,#"lasso"#,#"svmLinear"#  ,"relaxo"
               # ,"foba"
                #,
      # ,"svmPoly"
               # "kknn"
              #  ,"glmnet"
        #"glm"     ,
      #  "enet"
                #,
              #  "gbm"
                "rf"  #(mu lento)
              #  "knn"
          # "DENFIS"  (mu lento)
           #"randomGLM" 
          # "xgbTree" , perque no tira?
         #  ,"FIR.DM"
           #,
  #    "GFS.THRIFT"
          )
    
    ModelsLAT.list = as.list(vector(length = length(Models)))
    ModelsLONG.list = as.list(vector(length = length(Models)))
    
    metric <- "Rsquared"
    set.seed(123)
    
    # loop over the models selected to train
    for (z in 1:length(Models))
    {
      #z <- 1
     
      KnnControl <- caret::trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
                                 ) 
      #Enable parallel processing
      no_cores <- detectCores() - 3
      cl <- makeCluster(no_cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      
      ModelsLAT.list[[z]] <- 
      caret::train(LATITUDE ~ ., data=TrainLat, method=Models[z], 
            metric=metric, trControl=KnnControl)
    
    saveRDS(ModelsLAT.list[[z]],
            paste ("./models/" , approach, "Lat_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
    )
    
    #employed for assessing performance on unseen data
    # # ### defining train and test.
    # # 
    #  PredictedTest <- predict(ModelsLAT.list[[z]], newdata = TestLat)
    #  TempDF <-   postResample(PredictedTest , TestLat$LATITUDE)
    #  TempDF$Model <- Models[z]
    #  TempDF$Floor <- j
    #  TempDF$Building <- iteration
    #  TempDF$LongLat <- "Lat"
    #  TempDF$approach <- approach
    #  write.table(TempDF, file="PerformanceResults.csv", append=T, row.names=F, col.names=F,  sep=";")
    
    
    
    KnnControl <- trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
                               ) 
            ModelsLONG.list[[z]] <- 
              train(LONGITUDE ~ ., data=TrainLong, method=Models[z], 
                    metric=metric, trControl=KnnControl)
            
            saveRDS(ModelsLONG.list[[z]],
                    paste ("./models/" , approach, "Long_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
    )
    
            #employed for assessing performance on unseen data
            # PredictedTest <- predict(ModelsLONG.list[[z]], newdata = TestLong)
            # TempDF <-   postResample(PredictedTest , TestLong$LONGITUDE)
            # TempDF$Model <- Models[z]
            # TempDF$Floor <- j
            # TempDF$Building <- iteration
            # TempDF$LongLat <- "Long"
            # TempDF$approach <- approach
            # write.table(TempDF, file="PerformanceResults.csv", append=T, row.names=F, col.names=F,  sep=";", dec = ",")
            # 
            
            
    }          #loop of models

  }   #loop of floor
  
}   #loop of building











