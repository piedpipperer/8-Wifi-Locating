#Wifi Data Explorations
#Jordi Rojo
#12.12.2018


#### Working Directory ####
getwd()
getwd()

setwd("./8-Wifi-locating/")

source("1 - Inicialization and Libraries.R")

#first read and binding of data.
source("2 - Reading CSV - Wifi0.R")
#summary(Wifi0)

#melting and initial updates
source("3 - Melting&Update - WifiMelted.R")
# has the 0s, and has also dups which could not be eliminated, 
#they are after removing the 0s.

# eliminate 100s and duplicates for visualization:
Wifi4Visual <- WifiMelted %>% filter(SignalPow != 0) %>% unique() 
#esquisse::esquisser()

#writing csv with coordinates:
#source("99 - WritingSpatialCSV - AllWifiAgg.R")

#eliminate useles waps , Test Data  & convert the >-30 into normal signals 
#over WifiMelted
#source("4 - ElimWapsFromTrain - TrainWifi.R")
source("4 - CHEATING - ElimWapsFromTrain - TrainWifi.R")

# str(WifiMelted)   #10613720
# str(TrainWifi4BAndF) # 	4418440
# tail(TrainWifi4BAndF)
# str(TestWifi4BAndF) #6195280


# problematic WAPs of very powerful signals.
# WAP105 - 165544
# WAP62, 65 , 66 (phones 7 and 19)
# WAP 83, 87
# 
# WAP 12?
#   



  for (iteration in c("B0", "B1","B2")) 
{ 
  #iteration <- "B2"
  
  TrainFloorAlls <- 
    RemoveBotheringWAPs(WifiMelted %>% filter(BUILDINGID %in% c(iteration)) %>% select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
    )  %>% # unique()
    tidyr::spread(WAP, SignalPow, convert = FALSE)
  TrainFloorAlls$FLOOR <- factor(TrainFloorAlls$FLOOR)
  

  #lat and long trainning/validation.
  FloorsToIterate <- 
    TrainFloorAlls  %>% select(FLOOR) %>% #group_by(FLOOR) %>% ungroup() %>%
   unique()
  
  #source("8 - Trainning Floor.R")
  #as.vector(FloorsToIterate$FLOOR)


  for (j in as.vector(FloorsToIterate$FLOOR))
  {
    #iteration <- "B0"
    #j <- 'F2'
    
    TrainLong <- 
      RemoveBotheringWAPs(WifiMelted %>% filter(BUILDINGID %in% c(iteration)) %>% filter(FLOOR %in% c(j)) %>% select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
      )  %>% # unique()
      tidyr::spread(WAP, SignalPow, convert = FALSE)
   
    TrainLat <- TrainLong
    
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
    
    Models <- c(#
                #"vbmpRadial"
                #,
                #"lasso"
                #,
      #"svmRadial"
               # ,"svmPoly"
              #  ,"relaxo"
               # ,"foba"
                #,
                "kknn"
                #,
            #    "gbm"
                #,
          )
    ModelsLAT.list = as.list(vector(length = length(Models)))
    ModelsLONG.list = as.list(vector(length = length(Models)))
    
    metric <- "Rsquared"
    set.seed(123)
    for (z in 1:length(Models))
    {
      #z <- 1
     
      KnnControl <- trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
                                 ) 
      #### Enable parallel processing ####
      no_cores <- detectCores() - 3
      cl <- makeCluster(no_cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      
      ModelsLAT.list[[z]] <- 
      train(LATITUDE ~ ., data=TrainLat, method=Models[z], 
            metric=metric, trControl=KnnControl)
    
    saveRDS(ModelsLAT.list[[z]],
            paste ("./models/Lat_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
    )
    
    KnnControl <- trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
                               ) 
            ModelsLONG.list[[z]] <- 
              train(LONGITUDE ~ ., data=TrainLong, method=Models[z], 
                    metric=metric, trControl=KnnControl)
            
            saveRDS(ModelsLONG.list[[z]],
                    paste ("./models/Long_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
    )
    
    }          #loop of models


    
  }   #loop of floor
  
}   #loop of building


    

# # substituir per la llista de models...
# models <- resamples(list(#RF=rffit,
#                          Knn=Knnfit
#                          #,Svm=SvmFit
#                          ,gbm=gbmfit#, lm = lmdfd, svm = svmRdfd
# ))
# 
# bwplot(models, metric="Rsquared")
# bwplot(models, metric="RMSE")
# bwplot(models, metric="MAE")







## NN try
# TrainNN <- TrainLong 
# TrainNN$BUILDINGID <- NULL
# TrainNN$KeySample <- NULL
# TrainNN$FLOOR <- NULL
# #install.packages("keras")
# library(keras)
# install_keras()
# NN <- keras_model_sequential() 
# NN %>% 
#   layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   layer_dense(units = 128, activation = "relu") %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 10, activation = "softmax")
# #}
# NN %>% compile(
#   loss = "categorical_crossentropy",
#   optimizer = optimizer_rmsprop(),
#   metrics = c("accuracy")
# )
# 
# NNFit <- NN %>% fit(
#   TrainLat,#y_train, 
#   epochs = 30, batch_size = 128, 
#   validation_split = 0.2
# )
# 

    
    
    # #NN try:
    # library(neuralnet)
    # a <- as.formula(paste('LATITUDE+LONGITUDE ~ ' ,paste(colnames(TrainWifiFlor0Dum),collapse='+')))
    # Model = neuralnet(a ,  TrainWifiFlor0Dum #                  , lifesign="full"#, 12, stepmax=100000, rep=5, threshold=10
    # )
    # 
    # plot(Model)
    # Prediction = compute(Model, AllData@data[-fold,TrainingNames])
    





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









summary(TrainWifi)
summary(Utm)


range(TrainWifi@data$Lat)