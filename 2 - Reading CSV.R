
retrieve_last_file <- function(directory, key_word = "validationData") {
  
  #setwd(directory)
  #directory <- paste(getwd(),"/csv/",sep="")
  
  df <- file.info(
    list.files(directory, pattern = paste(key_word) )
  )
  
  if (df %>% count() >= 1) 
  {
    dataset <- read.csv(paste(directory, row.names(df %>% sample_n(1)),sep=""), header = T, sep = ",")
    return(dataset)
  }
  else 
  {
    return(FALSE)
  }
}

#test:
retrieve_last_file("/csv/","testData")
retrieve_last_file("D:/dropbox/Dropbox/ubiqum/8. Wifi Locating/8-Wifi-Locating/../","testAndtrain")
list.files("/csv/", pattern = paste("testData") )

# 1st preprocessing function

preprocess <- function(d1){
  
  TestWifi0 <- d1
  
  
  
  TestWifi0$LONGITUDE <- as.numeric(TestWifi0$LONGITUDE)
  TestWifi0$LATITUDE <- as.numeric(TestWifi0$LATITUDE)
  TestWifi0$FLOOR <- as.factor(TestWifi0$FLOOR)
  
  TestWifi0$BUILDINGID <- as.factor(TestWifi0$BUILDINGID)
  
  levels(TestWifi0$BUILDINGID) <- c("B0", "B1","B2")
  levels(TestWifi0$FLOOR) <- c("F0", "F1","F2","F3","F4")
  
  
  TestWifi0$SPACEID <- as.factor(TestWifi0$SPACEID)
  TestWifi0$RELATIVEPOSITION <- as.factor(TestWifi0$RELATIVEPOSITION)
  
  #left some part!

  return(TestWifi0)
  
}
