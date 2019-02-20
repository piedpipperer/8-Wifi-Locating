summary(is.na(TrainWifi))


library(tidyr)
# TrainWifi3 <- TrainWifiAllLocs %>% dplyr::select(LONGITUDE,LATITUDE,
#                                        FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                         , USERID
#                                         , PHONEID
#                                         ,TIMESTAMP
#                                         ,
#                                         WAP
#                                         ,SignalPow) %>% 
#   tidyr::spread(WAP, SignalPow, convert = FALSE) 

#my trainning set:
#TrainWifiAllLocsUnm

#summary(is.na(TrainWifi3))

#TrainWifi3 <- cbind(TrainWifiAgg$WAP,TrainWifiAgg$SignalPow)

str(TrainWifi3)


str(TrainWifi3)


#### lets try to predict Building, and make it very accurate ####
TrainWifiBuild <- TrainWifiAllLocsUnm  %>% ungroup()
#TrainWifiBuild <- cbind(TrainWifi3$WAP,TrainWifi3$SignalPow)

#TrainWifiBuild$BUILDING2 <- as.factor(TrainWifiBuild$BUILDING2)

TrainWifiBuild$LONGITUDE <- NULL
TrainWifiBuild$LATITUDE <- NULL
TrainWifiBuild$FLOOR <- NULL
TrainWifiBuild$SPACEID <- NULL
#TrainWifiBuild$BUILDINGID <- NULL
TrainWifiBuild$RELATIVEPOSITION <- NULL
#TrainWifiBuild$BUILDINGID <- as.numeric(TrainWifiBuild$BUILDINGID )


str(TrainWifiBuild)

#little test on how many waps are reduced: (fro the 300s)
# TestDF <- 
#   #WeHaveBuilding
#   TrainWifiAgg   %>%
#   filter(BUILDINGID #PredictedB 
#          %in% c("B2"))  

# GoodWAPs <- TestDF %>% group_by(
#   WAP) %>% 
#   summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
#   filter(SignalPow > 0)
# 
# str(GoodWAPs) 


BuildControl <- trainControl(method="repeatedcv"
                             ,classProbs = TRUE
                             #,savePredictions = TRUE
                             , number=5, repeats=2
)




#### Enable parallel processing ####
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

#WAPs<-grep("WAP", names(TrainWifiBuild), value=T)

library(randomForest)
# bestmtry_rf <- tuneRF(TrainWifiBuild[WAPs], TrainWifiBuild$BUILDINGID
#                     , ntreeTry=100,stepFactor=2
#                     ,improve=0.05,trace=TRUE
#                     #, plot=TRUE
#                     )


set.seed(123)
# RfBuildFit <- 
# train(BUILDINGID ~ ., data=TrainWifiBuild[WAPs], method="rf" 
#       , tuneGrid = 17
#       #tuneLength = 5,
#      # metric = "ROC",
#      , trControl=BuildControl
#      )

RfBuildFit <-
  #  system.time(
  randomForest(y=TrainWifiBuild$BUILDINGID,x=TrainWifiBuild[WAPs],importance=T
               ,method="rf", ntree=100, mtry=17)
#)


#saveRDS(RfBuildFit, "./models/BuildingRF.rds") 

library(xgboost)
set.seed(123)
XGBoostBuildFit <- train(BUILDINGID~.,
                         data = TrainWifiBuild,
                         method = "xgbTree",
                         tunelength = 5,
                         trControl = BuildControl#,tuneGrid=parametersGrid
)

#saveRDS(XGBoostBuildFit, "./models/BuildXGBoost.rds") 
saveRDS(XGBoostBuildFit, "./models/BuildXGBoost3.rds") 
XGBoostBuildFit <- readRDS("./models/BuildXGBoost3.rds")

XGBoostBuildFit
varImp(XGBoostBuildFit)
TestWifi
#varImp(XGBoostBuildFit)
#Models <- list.files(path = "./models/", pattern = "rds")


GenericTrClasses <- predict(XGBoostBuildFit, newdata = TrainWifiBuild)
Matrix <-   confusionMatrix(GenericTrClasses, TrainWifiBuild$BUILDINGID)

GenericTestClasses <- predict(XGBoostBuildFit, newdata = TestWifi)
Matrix <-   confusionMatrix(GenericTestClasses, TestWifi$BUILDINGID)



GenericTestClasses <- predict(RfBuildFit, newdata = TestWifi)
Matrix <-   confusionMatrix(GenericTestClasses, TestWifi$BUILDINGID)



test1 <- TrainWifiBuild
test1$PredictedB <-  GenericTrClasses

test2 <- TestWifi
test2$PredictedB <-  GenericTestClasses

#esquisse::esquisser()


TestProbs <- predict(RfBuildFit, 
                     newdata = TestWifi, type = "prob")

TrainProbs <- predict(RfBuildFit, 
                      newdata = TrainWifiBuild, type = "prob")

# TestProbs
# TrainProbs

summary(
  TrainWifiBuild
)
# 