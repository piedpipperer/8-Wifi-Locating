

RemoveBotheringWAPs <- function(TrainDF) {
  
  GoodWAPs <- TrainDF %>% group_by(
    WAP) %>% 
    summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
    filter(SignalPow > 0)
  
  return(TrainDF %>% 
           subset(WAP %in% GoodWAPs$WAP))
}

WifiMelting <- function(Wifi0) {

  TmpWifi <- Wifi0 %>% unique()
  TmpWifi$KeySample <-  1:nrow(TmpWifi) 
  TempWifiMelted <- TmpWifi %>% unique() %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                                               ,"RELATIVEPOSITION","USERID","PHONEID",
                                                               "TIMESTAMP", "TEST","KeySample")
  )  %>% 
    mutate( value = value + 105) %>% 
    mutate( value = case_when(value == 205 ~ 0,
                              value > 75 & value < 200 ~ value - 40,
                              TRUE ~ value) 
    )  #filter( value != 100) %>% 
  
  colnames(TempWifiMelted)[12] <- "WAP"
  colnames(TempWifiMelted)[13] <- "SignalPow"
  
  TempWifiMelted$KEY <- 1:nrow(TempWifiMelted) 
  #10613644
  
  
 return(
  # RemoveBotheringWAPs(
     TempWifiMelted
  #   ) 
 )
  # %>% select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID, WAP, SignalPow, KeySample, KEY)
  
  
}

