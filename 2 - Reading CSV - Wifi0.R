
TrainWifi0 <- read.csv("../csv/trainingData.csv")
TestWifi0 <- read.csv("../csv/validationData.csv")

TestWifi0$LONGITUDE <- as.numeric(TestWifi0$LONGITUDE)
TestWifi0$LATITUDE <- as.numeric(TestWifi0$LATITUDE)

TrainWifi0$LONGITUDE <- as.numeric(TrainWifi0$LONGITUDE)
TrainWifi0$LATITUDE <- as.numeric(TrainWifi0$LATITUDE)

TrainWifi0$FLOOR <- as.factor(TrainWifi0$FLOOR)
TestWifi0$FLOOR <- as.factor(TestWifi0$FLOOR)

TrainWifi0$TEST <- FALSE 
TestWifi0$TEST <- TRUE 
Wifi0 <- rbind(TrainWifi0,TestWifi0)