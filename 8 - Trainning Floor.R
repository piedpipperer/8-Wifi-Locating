




IterateDF2 <- TakeNumberUniqueLocations(TrainFloorAlls,18)
TestIterate <- TrainFloorAlls %>%
  subset(!KeySample %in% IterateDF2$KeySample)

IterateDF3 <- IterateDF2 %>% ungroup()
IterateDF3$LONGITUDE <- NULL
IterateDF3$LATITUDE <- NULL

IterateDF3$KeySample <- NULL
IterateDF3$BUILDINGID <- NULL
#IterateDF3 %>% group_by(FLOOR) %>%    summarise(n = n()) %>% arrange(n)


FloorControl <- trainControl(method="repeatedcv"
                             ,classProbs = TRUE
                             #,savePredictions = TRUE
                             , number=5#, repeats=3
)

library(xgboost)
set.seed(123)
XGBoostFloorFit <- train(FLOOR~.,
                         data = IterateDF3,
                         method = "xgbTree",
                         tunelength = 5,
                         trControl = FloorControl#,tuneGrid=parametersGrid
)

saveRDS(XGBoostFloorFit,
paste ("./models/FloorXGBoost_" , iteration , ".rds", sep = "")
)



XGBoostFloorFit <- readRDS(paste ("./models/FloorXGBoost_" , iteration , ".rds", sep = ""))

#això ho hem de fer abans d'asignar null a les variables 

#3k1 
#varImp(XGBoostFloorFit)
#Models <- list.files(path = "./models/", pattern = "rds")

GenericTrClasses <- predict(XGBoostFloorFit, newdata = TrainWifiFloor)
Matrix <-   confusionMatrix(GenericTrClasses, TrainWifiFloor$FLOOR)

GenericTestClasses <- predict(XGBoostFloorFit, newdata = TestIterate)
Matrix <-   confusionMatrix(GenericTestClasses, TestIterate$FLOOR)
