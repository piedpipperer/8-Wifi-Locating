
TrainWifi0 <- read.csv("../csv/trainingData.csv")
TestWifi0 <- read.csv("../csv/validationData.csv")

TestWifi0$LONGITUDE <- as.numeric(TestWifi0$LONGITUDE)
TestWifi0$LATITUDE <- as.numeric(TestWifi0$LATITUDE)

TrainWifi0$LONGITUDE <- as.numeric(TrainWifi0$LONGITUDE)
TrainWifi0$LATITUDE <- as.numeric(TrainWifi0$LATITUDE)

TrainWifi0$FLOOR <- as.factor(TrainWifi0$FLOOR)
TestWifi0$FLOOR <- as.factor(TestWifi0$FLOOR)


TestWifi0$TEST <- TRUE 
TrainWifi0$TEST <- FALSE 


Wifi0 <- rbind(TrainWifi0,TestWifi0)
Wifi0$BUILDINGID <- as.factor(Wifi0$BUILDINGID)

levels(Wifi0$BUILDINGID) <- c("B0", "B1","B2")

Wifi0$SPACEID <- as.factor(Wifi0$SPACEID)
Wifi0$RELATIVEPOSITION <- as.factor(Wifi0$RELATIVEPOSITION)
