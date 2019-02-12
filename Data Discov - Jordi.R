#Wifi Data Explorations
#Jordi Rojo
#12.12.2018


#### Working Directory ####
getwd()
getwd()


#strsplit(version[['version.string']], ' ')[[1]][3]


#install.packages("Rserve")
library(esquisse)



#### Load Libraries ####
library(pacman)
pacman::p_load(readr, caret, plotly, ggplot2, 
               labeling, promises, ggridges, 
               doParallel, mlbench,# inum, e1071, 
               corrplot#, ggpubr
               , rpart, rpart.plot, gbm
               , boot, dplyr,
               reshape, Rserve#, tidyvers
               , padr)

#library(proj4)

#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

TrainWifi0 <- read.csv("./csv/trainingData.csv")
TestWifi0 <- read.csv("./csv/validationData.csv")

TestWifi0$LONGITUDE <- as.numeric(TestWifi0$LONGITUDE)
TestWifi0$LATITUDE <- as.numeric(TestWifi0$LATITUDE)

TrainWifi0$LONGITUDE <- as.numeric(TrainWifi0$LONGITUDE)
TrainWifi0$LATITUDE <- as.numeric(TrainWifi0$LATITUDE)

TrainWifi0$FLOOR <- as.factor(TrainWifi0$FLOOR)
TestWifi0$FLOOR <- as.factor(TestWifi0$FLOOR)


str(TrainWifi0) #19937

TrainWifi <- TrainWifi0 %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                  ,"RELATIVEPOSITION","USERID","PHONEID",
                                   "TIMESTAMP")
     ) %>% #filter( value != 100) %>% 
  mutate( value = value + 105) %>% 
  mutate( value = case_when(value == 205 ~ 0,
                            TRUE ~ value) 
  )


TestWifi <-   melt(TestWifi0
                   , id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                 ,"RELATIVEPOSITION","USERID","PHONEID",
                               "TIMESTAMP")
  ) %>% #filter( value != 100) %>% 
  mutate( value = value + 105)  %>% 
  mutate( value = case_when(value == 205 ~ 0,
                            TRUE ~ value) 
  )
#str(TestWifi)

#taken the 100s,  
#str(TrainWifiD) #10.008.477

colnames(TrainWifi)[10] <- "WAP"
colnames(TrainWifi)[11] <- "SignalPow"

colnames(TestWifi)[10] <- "WAP"
colnames(TestWifi)[11] <- "SignalPow"


TestWifi$SignalPow = as.double(TestWifi$SignalPow)


str(TrainWifi) #
#summary(is.na(TrainWifi$LATITUDE))

summary(TrainWifi)

str(TrainWifi) #10367240
str(TestWifi)

GoodWAPs <- TrainWifi %>% group_by(
  #, USERID
  #, PHONEID
  #timestamp...
  #,
  WAP) %>% 
  summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
  filter(SignalPow > 0)

#### eliminating useless WAPs ####
TempDF <- TestWifi %>% filter(SignalPow != 0) %>% dplyr::select(WAP)
TrainWifi2 <- TrainWifi %>% subset(WAP %in% TempDF$WAP)  %>% subset(WAP %in% GoodWAPs$WAP) 
#same should be done for Test Set....

WAP105 - 165544
#not necessary
# TempDF2 <- TrainWifi %>% filter(SignalPow != 0) %>% dplyr::select(WAP)
# TestWifi2 <- 
#   TestWifi %>% subset(WAP %in% TempDF2$WAP)

str(TrainWifi2)


esquisse::
#### uniforming location disstributions ####
         
str(TrainWifi2)
TrainWifiAgg <- TrainWifi2 %>% group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                        #, USERID
                                        #, PHONEID
                                        #timestamp...
                                        , WAP) %>% 
  summarize(SignalPow = as.numeric(mean(SignalPow)))




#### Dummify the data ####
# TrainWifiFlor0 <- TrainWifi2  %>% filter(FLOOR == 0)  %>% 
#   select(LONGITUDE,LATITUDE,SignalPow,WAP)
# dfdummy <- dummyVars(" ~ .", data = TrainWifiFlor0)
# 
# str(TrainWifiFlor0)
# 
# TrainWifiFlor0Dum <- data.frame(predict(dfdummy, newdata = TrainWifiFlor0))
# str(TrainWifiFlor0Dum)





library(tidyr)
TrainWifi3 <- TrainWifi2  %>%
  tidyr::spread(WAP, SignalPow, convert = FALSE) 

TestWifi2 <- TestWifi %>%  tidyr::spread(WAP, SignalPow) 


str(TestWifi2)
str(TrainWifi3)


str(TrainWifi3)



#### lets try to predict floor, and make it very accurate ####
TrainWifiFloor <- TrainWifi3
TrainWifiFloor$LONGITUDE <- NULL
TrainWifiFloor$LATITUDE <- NULL
TrainWifiFloor$BUILDINGID <- NULL
TrainWifiFloor$SPACEID <- NULL
TrainWifiFloor$RELATIVEPOSITION <- NULL

TrainWifiFloor$FLOOR <- as.factor(TrainWifiFloor$FLOOR)

FloorControl <- trainControl(method="repeatedcv"
                             #,classProbs = TRUE
                        , number=9, repeats=2)


RfFloorFit <- 
  train(FLOOR ~ ., data=TrainWifiFloor, method="rf", 
        #metric=metric,
        trControl=FloorControl)

saveRDS(RfFloorFit, "FloorRF.rds") 

install.packages("xgboost")
library(xgboost)

set.seed(123)
XGBoostFloorFit <- train(FLOOR~., 
                      data = TrainWifiFloor,
                      method = "xgbTree",
                      trControl = FloorControl#,tuneGrid=parametersGrid
                    )
saveRDS(XGBoostFloorFit, "FloorXGBoost.rds") 
#?make.names
#make.names("FLOOR",unique= TRUE)


C50FloorFit <- 
  train(FLOOR ~ ., data=TrainWifiFloor, method="C5.0", 
        #metric=metric,
        trControl=FloorControl)


saveRDS(C50FloorFit, "C50Floor.rds") 

### to transform in loop sometime in future ####

TestWifiFlor0Dum <- TestWifi3  %>% filter(FLOOR == 0) 
#%>% spread(WAP,SignalPow) #%>% 
#select(LONGITUDE,LATITUDE,SignalPow,WAP) %>% spread(WAP,SignalPow) #group_by(LONGITUDE,LATITUDE,WAP)

TrainWifiFlor0Dum$USERID <- NULL
#TrainWifiFlor0Dum$PHONEID <- NULL
#TrainWifiFlor0Dum$TIMESTAMP <- NULL
TrainWifiFlor0Dum$RELATIVEPOSITION <- NULL
TrainWifiFlor0Dum$SPACEID <- NULL
TrainWifiFlor0Dum$BUILDINGID <- NULL
TrainWifiFlor0Dum$FLOOR <- NULL


str(TrainWifiFlor0Dum)


metric <- "Rsquared"
seed <- 123
set.seed(seed)


#### Conventional Algoritmsss ####
control <- trainControl(method="repeatedcv", number=9, repeats=2)

DTDF <- TrainWifiFlor0Dum # %>% filter(FLOOR == 0 & SignalPow != 0) 
DTDF$LATITUDE <- NULL
DTDF$FLOOR <- NULL
DTDF$BUILDINGID <- NULL
DTDF$SPACEID <- NULL
DTDF$RELATIVEPOSITION <- NULL
str(DTDF)


#gbm
gbmfit <- caret::train(LONGITUDE ~ ., data=DTDF, method="gbm", 
                metric=metric, trControl=control)

# dt <- rpart(LONGITUDE~., data=GbmDF, cp=.001, model = TRUE)
# rpart.plot(dt, box.palette="RdBu", shadow.col="gray", nn=TRUE)
# 
varImp(gbmfit, scale = FALSE)




#rf
seed <- 123
set.seed(seed)
rffit <- 
  train(LONGITUDE ~ ., data=DTDF, method="rf", 
                        metric=metric, trControl=control)

#knn
seed <- 123
set.seed(seed)
Knnfit <- 
  train(LONGITUDE ~ ., data=DTDF, method="kknn", 
        metric=metric, trControl=control)

varImp(Knnfit, scale = FALSE)
# 
# gbmdfit <-  gbm(LONGITUDE ~ .#, distribution = "bernoulli"
#                 ,    data = GBMSubset#, weights
#     , var.monotone = NULL, 
#     n.trees = 1,
#     interaction.depth = 30, n.minobsinnode = 10, shrinkage = 0.1,
#     bag.fraction = 0.5, train.fraction = 1, cv.folds = 0,
#     keep.data = TRUE
#     , verbose = TRUE
#     , class.stratify.cv = NULL
#     ,  n.cores = 3
#     )


models <- resamples(list(rf=rffit,Knn=Knnfit,
  gbm=gbmdfit#, lm = lmdfd, svm = svmRdfd
))

bwplot(models, metric="Rsquared")
bwplot(models, metric="RMSE")
bwplot(models, metric="MAE")


str(TestWifi2)


# TestWifi3 <- TestWifi2
# 
# TestWifi3$USERID <- NULL
# #TrainWifiFlor0Dum$PHONEID <- NULL
# #TrainWifiFlor0Dum$TIMESTAMP <- NULL
# TestWifi3$RELATIVEPOSITION <- NULL
# TestWifi3$SPACEID <- NULL
# TestWifi3$BUILDINGID <- NULL
# TestWifi3$FLOOR <- NULL

str(TestWifi2)

TestWifi3 <- TestWifi2 %>% filter(FLOOR == 0) 


gbmPr <- predict(gbmfit, TestWifi3)
rfmPr <- predict(rffit, TestWifi3)
KnnPr <- predict(Knnfit, TestWifi3)

postResample(gbmPr, TestWifi3$LONGITUDE)
postResample(rfmPr, TestWifi3$LONGITUDE)
postResample(KnnPr, TestWifi3$LONGITUDE)
#gbmpr <- predict(gbmfit, DTDF)







#### trying polinomial regressions ####
PoliSubset1 <- subset(TrainWifiFlor0Dum, select = c("LONGITUDE"#,"LATITUDE"
                                                    ))

#PoliSubset1$LATITUDE <- as.numeric(PoliSubset1$LATITUDE)
PoliSubset1$LONGITUDE <- as.Numeric(PoliSubset1$LONGITUDE)
str(PoliSubset1)

PoliSubset2 <- TrainWifiFlor0Dum
PoliSubset2$LATITUDE <- NULL
PoliSubset2$LONGITUDE <- NULL
str(PoliSubset2)

test <- 
as.matrix(PoliSubset1)
test2 <- 
  as.matrix(PoliSubset2)

library(glmnet)
PoliFit <- cv.glmnet(test2,test,type.measure="mae",alpha=0#,family="multinomial"
                     )

# 
# PoliPredict <- predict (PoliFit,s=PoliFit$lambda
#                         )






##### trainning 1st try ####
control <- trainControl(method="repeatedcv", number=9, repeats=2)


set.seed(seed)

#ToPredict <- TrainWifiFlor0Dum %>% select(LONGITUDE,LATITUDE)




#SAR model
library(maptools)
library(spdep)
columbus <- readShapePoly(system.file("etc/shapes/columbus.shp",
                                      package="spdep")[1])




#svm  (me engaña)
library( e1071)
svmdfd <- svm(LONGITUDE+LATITUDE ~., data=TrainWifiFlor0Dum, 
              metric=metric, trControl=control, scale = TRUE)

svmpred <- predict(svmdfd, TrainWifiFlor0Dum)
svmpreddf <- data.frame(svmpred)
tail(svmpreddf)
str(svmpreddf)
svmpredfull <- cbind(TrainWifiFlor0Dum, svmpreddf)

subset4 <- subset(svmpredfull, select = c("ProductType","dfndpr","Volume"))


str(svmpred)






#KNN TRY NOT WORKING!!
KNNTrain <-train(LONGITUDE ~  ., data = TrainWifiFlor0Dum, method = "kknn", 
             metric = metric, trControl = control)

prc_test_pred <- knn(train = prc_train#, test = prc_test
                     ,cl = prc_train_labels, k=4)

str(ToPredict)





#NN try:
library(neuralnet)
a <- as.formula(paste('LATITUDE+LONGITUDE ~ ' ,paste(colnames(TrainWifiFlor0Dum),collapse='+')))
Model = neuralnet(a ,  TrainWifiFlor0Dum #                  , lifesign="full"#, 12, stepmax=100000, rep=5, threshold=10
                  )

plot(Model)
Prediction = compute(Model, AllData@data[-fold,TrainingNames])








#### Convert of AggInfo into grades ####
#lets try an agg version of the data: 

# TestWifiAgg <- TestWifi %>%  group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                        #, USERID
#                                        , PHONEID
#                                        #timestamp...
#                                        , WAP) %>% 
#   summarize(SignalPow = mean(SignalPow))
# 
# TrainWifiAgg2 <- TrainWifi2 %>% group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                         #, USERID
#                                         , PHONEID
#                                         #timestamp...
#                                         , WAP) %>% 
#   summarize(SignalPow = median(SignalPow)) %>% filter (SignalPow != 0)

TestWifiAgg <- TestWifi %>%  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                       #, USERID
                                       , PHONEID
                                       #timestamp...
                                       , WAP
                                       , SignalPow) %>% filter (SignalPow != 0)

TrainWifiAgg2 <- TrainWifi2 %>% dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                         #, USERID
                                         , PHONEID
                                         #timestamp...
                                         , WAP
                                         , SignalPow)  %>% filter (SignalPow != 0 ) 



str(TestWifi)
str(TrainWifi2)

TrainWifiAgg2$TestOrTrain <- "TRAIN"
TestWifiAgg$TestOrTrain <- "TEST"

AllWifiAgg <- rbind(TestWifiAgg,TrainWifiAgg2)
str(AllWifiAgg)
esquisse::esquisser()


#density for power!
ggplot(data = TrainWifi) +
  aes(x = SignalPow) +
  geom_density(adjust = 1, fill = "#0c4c8a") +
  theme_minimal()



library(rgdal)
library(raster)

proj4string <- "+proj=utm +zone=31t +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

proj4stringT <- "+proj=longlat +zone=31t +north  "

AllWifiAgg$FLOOR <- as.numeric(AllWifiAgg$FLOOR) * 10

# TEST <- TrainWifi %>% select(LONGITUDE,LATITUDE)
# 
# colnames(TEST)[1] <- "x"
# colnames(TEST)[2] <- "y"
# str(TEST)
# 
# pj <- rgdal::project(TEST, proj4string#, inverse=TRUE
#               )


#str(TrainWifi2)
Look4Out <- TrainWifi2 %>% dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                         , USERID
                                         , PHONEID, SignalPow
                                         #timestamp...
                                         , WAP) %>% filter (SignalPow != 0) %>% 
                                  filter (SignalPow > 30)

# Look4Out <- TrainWifi2 %>% filter (SignalPow > 30)


Look4Out$TIMESTAMP <- NULL
Look4Out$USERID <- NULL
Look4Out$RELATIVEPOSITION <- NULL



str(Look4Out)
summary((Look4Out))
coordinates(Look4Out) <- c("LONGITUDE", "LATITUDE")
coordinates(TrainWifiAgg2) <- c("LONGITUDE", "LATITUDE")
coordinates(AllWifiAgg) <- c("LONGITUDE", "LATITUDE")


summary(Look4Out)
#TrainWifi$LATITUDE
proj4string(Look4Out) <- CRS(proj4string)
proj4string(TrainWifiAgg2) <- CRS(proj4string)
proj4string(AllWifiAgg) <- CRS(proj4string)

#!is.numeric(TrainWifi)

#validObject(TrainWifi)

# 
# print(
# project(TrainWifi,proj4string)
# )

# select the appropiate dataframe to store..


Utm <- spTransform(AllWifiAgg, proj4stringT)
Utm4Out <- spTransform(Look4Out, proj4stringT)
Utm$LONGITUDE
Utm$LATITUDE

Utm$optional <- NULL 

#write.csv2(Utm,"testAndtrain2.csv") # this is for the kepler!!
#write.csv(Utm,"testAndtrain.csv") # this is for the kepler!!

write.csv2(Utm,"./csv/train2.csv")
# write.csv2(Utm4Out,"./csv/looking4Outliers2.csv")
# write.csv(Utm4Out,"./csv/looking4Outliers.csv")


summary(TrainWifi)
summary(Utm)


range(TrainWifi@data$Lat)