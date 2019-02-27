


approach <- ""


for (iteration in c(#"B0",
                    #"B1",
  "B2")) 
{ 
#iteration <- "B0"

# IterateDF1 <- 
#   TrainWifiAgg %>% dplyr::select(BUILDINGID
#                                  #, USERID
#                                  #, PHONEID
#                                  #timestamp...
#                                  ,
#                                  WAP
#                                  ,SignalPow) %>%
#   filter(BUILDINGID %in% c(iteration)) 


#%>% 
#gather(variable, value, -WAP) 

TrainWifiFloor <- TakeNumberUniqueLocations(
 RemoveBotheringWAPs(WifiMelted2  %>%
                                         filter(BUILDINGID %in% c(iteration))   %>% ungroup() %>% 
   dplyr::select(FLOOR,BUILDINGID,WAP,SignalPow,LONGITUDE,LATITUDE,KeySample)
   ) %>%
   tidyr::spread(WAP, SignalPow, convert = FALSE) 
,15) 

TrainWifiFloor$KeySample <- NULL
TrainWifiFloor$LATITUDE <- NULL
TrainWifiFloor$LONGITUDE <- NULL

#TrainWifiAllLocs?  not being able to remove bothering waps.


#put to null the uninteresting variables? but wait, because we will use this for every iterarion.
TrainWifiFloor$FLOOR <- factor(TrainWifiFloor$FLOOR)
FloorControl <- trainControl(method="repeatedcv"
                             ,classProbs = TRUE
                             #,savePredictions = TRUE
                             , number=5#, repeats=3
)


#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))


set.seed(123)



# library(xgboost)
# set.seed(123)
XGBoostFloorFit <- train(FLOOR~.,
                         data = TrainWifiFloor,
                         method = "xgbTree",
                         tunelength = 3, #5,
                         trControl = FloorControl#,tuneGrid=parametersGrid
)

# test <- predict(XGBoostFloorFit, newdata = ValidationSet %>%    filter(BUILDINGID == iteration) )
#  
saveRDS(XGBoostFloorFit, 
        paste ("./models/FloorXGBoost_" , iteration , ".rds", sep = "")
) 

#XGBoostFloorFit <- readRDS("./models/FloorXGBoost.rds")

#varImp(XGBoostFloorFit)
#Models <- list.files(path = "./models/", pattern = "rds")

# GenericTrClasses <- predict(XGBoostFloorFit, newdata = TrainWifiFloor)
# Matrix <-   confusionMatrix(GenericTrClasses, TrainWifiFloor$FLOOR)

# GenericTestClasses <- predict(XGBoostFloorFit, newdata = TestWifi  %>%
#                                 filter(BUILDINGID %in% c(iteration)))
# Matrix <-   confusionMatrix(GenericTestClasses, TestWifi  %>%
#                               filter(BUILDINGID %in% c(iteration)) %>% select(FLOOR)
# )



}