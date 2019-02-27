#Wifi Data Explorations
#Jordi Rojo
#12.12.2018

#to later use to run all code, and store csvs and models with this name in beginning

#### Working Directory ####
getwd()
getwd()

#falta implementar la escritura en ficheros?

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

#esquisse::esquisser()

#writing csv with coordinates:
#Wifi4Visual <- WifiMelted %>% filter(SignalPow != 0) %>% unique()
#source("99 - WritingSpatialCSV - AllWifiAgg.R")


# str(WifiMelted)   #10613720
# str(TrainWifi4BAndF) # 	4418440
# tail(TrainWifi4BAndF)yes
# str(TestWifi4BAndF) #6195280


# problematic WAPs of very powerful signals.
# WAP105 - 165544
# WAP62, 65 , 66 (phones 7 and 19)
# WAP 83, 87
# 
# WAP 12?
#   


#approach <- "Minimal"
approach <- ""   #1st approach.
#approach <- "OnlyTrain"

source("4 - CHEATING - ElimWapsFromTrain - TrainWifi.R")

summary(
  WifiMelt2) #5211000   299970 (Minimal approach)
# 10036000  
# 577720    (T&T approach)

if (approach == "OnlyTrain")
{ 
  WifiMelted2 <- WifiMelted %>% filter(TEST == FALSE)
} else {
  WifiMelted2 <- WifiMelt2
}









for (iteration in c(
   "B0"
  #                   ,
  #                   "B1"
  #                   ,
  #"B2"
)) 
{ 
  #iteration <- "B2"
  
  TrainFloorAlls <- 
    RemoveBotheringWAPs(WifiMelted2 %>% filter(BUILDINGID %in% c(iteration)) %>% dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
    )  %>% # unique()
    tidyr::spread(WAP, SignalPow, convert = FALSE)
  TrainFloorAlls$FLOOR <- factor(TrainFloorAlls$FLOOR)
  
  
  #lat and long trainning/validation.
  FloorsToIterate <- 
    TrainFloorAlls  %>% dplyr::select(FLOOR) %>% #group_by(FLOOR) %>% ungroup() %>%
    unique()
  
  #source("8 - Trainning Floor.R")
  #as.vector(FloorsToIterate$FLOOR)
  
  

  for (j in # as.vector(FloorsToIterate$FLOOR) 
       c(
       "F0"#,
      # "F2"
         #,
      #   "F4"
       )
  )
  {
    #iteration <- "B0"
    #j <- 'F2'
    
    TrainLong <- TakeNumberUniqueLocations(
      RemoveBotheringWAPs(WifiMelted2 %>% filter(BUILDINGID %in% c(iteration)) %>% filter(FLOOR %in% c(j)) %>% dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
      )  %>% # unique()
      tidyr::spread(WAP, SignalPow, convert = FALSE) 
    #,20)
    ,10)  #used for B2 and F4
    
    TrainLat <- TrainLong
    # 
     TestLong <-  WifiUnMelt  %>% filter(BUILDINGID %in% c(iteration)) %>% filter(FLOOR %in% c(j))  %>%
       subset(!KeySample %in% TrainLong$KeySample)
     
     TestLat <- TestLong
    

    # TrainNN <- TrainLong
    # TrainNN$BUILDINGID <- NULL
    # TrainNN$KeySample <- NULL
    # TrainNN$FLOOR <- NULL
    
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
      #
      #"kknn"
      #,"glmnet"
      #"glm"     ,
      #  "enet"
      #,
      #  "gbm"
      #,
    #  "rf"  #(mu lento)
        "knn"
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
    
    
    for (z in 1:length(Models))
    {
      #z <- 1
      
      KnnControl <- caret::trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
      ) 
      #### Enable parallel processing ####
      no_cores <- detectCores() - 3
      cl <- makeCluster(no_cores)
      registerDoParallel(cl)
      on.exit(stopCluster(cl))
      # 
      ModelsLAT.list[[z]] <-
        caret::train(LATITUDE ~ ., data=TrainLat, method=Models[z],
                     metric=metric, trControl=KnnControl)

      saveRDS(ModelsLAT.list[[z]],
              paste ("./models/" , approach, "Lat_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
      )

      # ### defining train and test.
      #
       PredictedTest <- predict(ModelsLAT.list[[z]], newdata = TestLat)
       TempDF <-   postResample(PredictedTest , TestLat$LATITUDE)
       TempDF$Model <- Models[z]
       TempDF$Floor <- j
       TempDF$Building <- iteration
       TempDF$LongLat <- "Lat"
       TempDF$approach <- approach
       write.table(TempDF, file="PerformanceResults.csv", append=T, row.names=F, col.names=F,  sep=";")
      #

      KnnControl <- trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
      )
      ModelsLONG.list[[z]] <-
        train(LONGITUDE ~ ., data=TrainLong, method=Models[z],
              metric=metric, trControl=KnnControl)

      PredictedTest <- predict(ModelsLONG.list[[z]], newdata = TestLong)
      TempDF <-   postResample(PredictedTest , TestLong$LONGITUDE)
      TempDF$Model <- Models[z]
      TempDF$Floor <- j
      TempDF$Building <- iteration
      TempDF$LongLat <- "Long"
      TempDF$approach <- approach
      write.table(TempDF, file="PerformanceResults.csv", append=T, row.names=F, col.names=F,  sep=";", dec = ",")

      saveRDS(ModelsLONG.list[[z]],
              paste ("./models/" , approach, "Long_" , Models[z] , "_" , j , "_" , iteration , ".rds", sep = "")
      )
      
    }          #loop of models
    
  }   #loop of floor
  
}   #loop of building











#lets try multilasso: 
TrainSetLasso <- TrainNN
TrainSetLasso$LONGITUDE <- NULL
TrainSetLasso$LATITUDE <- NULL

library(glmnet)
mfit = glmnet(as.matrix(TrainSetLasso),as.matrix( TrainNN %>% dplyr::select(LATITUDE,LONGITUDE
)), family = "mgaussian")
plot(mfit, xvar = "lambda", label = TRUE, type.coef = "2norm")

predict(mfit, newx = x[1:5,], s = c(0.1, 0.01))

cvmfit = cv.glmnet(as.matrix(TrainSetLasso), as.matrix( TrainNN %>% dplyr::select(LATITUDE,LONGITUDE
)), family = "mgaussian")

plot(cvmfit)
#nothing very special :(

MultiTaskControl <- caret::trainControl(method="repeatedcv", number=9, repeats=2#, scale = TRUE
) 
#### Enable parallel processing ####
no_cores <- detectCores() - 3
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))

ModelsLAT.list[[z]] <- 
  caret::train(LATITUDE ~ ., data=TrainLat, method=Models[z], 
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
        
        
        
        
        
        
        
        
        
        
        ## NN investigation
        # 
        # library(reticulate)
        # use_condaenv("py37")
        # py_config()
        # import("numpy")
        # np<-import("numpy")
        # print(np$version$full_version)
        
        #devtools::install_github("rstudio/keras")
        #install.packages("kerasR")
        #install_tensorflow()
        #install_keras()
        #TrainNN
        library(sp)
        library(tensorflow)
        library(kerasR)
        library(keras)
        
        reticullate::py_initialize()
        reticulate::py_discover_config()
        
        
        #reticulate::use_condaenv("C:/Users/JORDI/Anaconda3/")
        reticulate::use_condaenv("C:/Users/JORDI/Anaconda3/envs/r-tensorflow")#,"1.15.4")
        #use_python("C:/Users/JORDI/Anaconda3/envs/r-tensorflow/")
        
        #Sys.setenv(RETICULATE_PYTHON = "C:/Users/JORDI/Anaconda3/envs/r-tensorflow/")
        Sys.setenv(PATH= paste("C:/Users/JORDI/Anaconda3/envs/r-tensorflow/Lib"))
        #reticulate::use_python("C:/Users/JORDI/Anaconda3/envs/r-tensorflow/")
        
        reticulate::py_numpy_available()
        reticulate::py_module_available("numpy")
        reticulate::py_module_available("tensorflow")
        reticulate::py_module_available("keras")
        # all ok.
        kerasR::keras_init()
        
        # installation of Numpy >= 1.6 not found
        
        
        TrainNN <-   RemoveBotheringWAPs(WifiMelted %>% filter(BUILDINGID %in% c("B0")) %>% filter(FLOOR %in% c("F0")) %>% dplyr::select(BUILDINGID,LONGITUDE,LATITUDE,FLOOR, WAP, SignalPow, KeySample) 
        )  %>% # unique()
          tidyr::spread(WAP, SignalPow, convert = FALSE)
        
        #train_id <- train$ID
        TrainNNLabels <- TrainNN %>% select(LATITUDE, LONGITUDE)
        TrainNNGood <- TrainNN
        TrainNNGood$LATITUDE <- NULL
        TrainNNGood$LONGITUDE <- NULL
        TrainNNGood$KeySample <- NULL
        TrainNNGood$FLOOR <- NULL
        TrainNNGood$BUILDINGID <- NULL
        #ValidationSet
        
        NN <- keras::keras_model_sequential()
        
        NN %>%
          layer_dense(units = ncol(TrainNNGood), activation = "relu", kernel_initializer='normal') %>%
          layer_dense(units = 36, activation = "relu", kernel_initializer='normal') %>% 
          layer_dense(units = 6, activation = "relu", kernel_initializer='normal') %>% 
          layer_dense(units = 2, activation = "relu", kernel_initializer='normal') 
        
        
        
        NN %>% compile(
          optimizer = optimizer_adam(),
          loss = 'mse')
        
        
        
        train.ind <- sample(1:nrow(TrainNNGood), 0.95*nrow(TrainNNGood))
        
        #normalize!!!
        ****
          NN %>% fit(as.matrix(TrainNNGood[train.ind,])
                     , as.matrix(TrainNNLabels[train.ind,])
                     , epochs=100, batch_size=128)
        
        summary(NN)
        
        score <- NN %>% evaluate(as.matrix(TrainNNGood[-train.ind,])
                                 , TrainNNLabels[-train.ind], batch_size = 128)
        
        
        #  validation set.
        # models <- resamples(list(rf=rffit,Knn=Knnfit,
        #   gbm=gbmdfit#, lm = lmdfd, svm = svmRdfd
        # ))
        # 
        # bwplot(models, metric="Rsquared")
        # bwplot(models, metric="RMSE")
        # bwplot(models, metric="MAE")
        
        
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
        
        
        
        
        
        
        
        
        
        #NN try:
        library(neuralnet)
        a <- as.formula(paste('LATITUDE+LONGITUDE ~ ' ,paste(colnames(TrainWifiFlor0Dum),collapse='+')))
        Model = neuralnet(a ,  TrainWifiFlor0Dum #                  , lifesign="full"#, 12, stepmax=100000, rep=5, threshold=10
        )
        
        plot(Model)
        Prediction = compute(Model, AllData@data[-fold,TrainingNames])
        
        
        