

RemoveBotheringWAPs <- function(TrainDF) {
  
  GoodWAPs <- TrainDF %>% group_by(
    WAP) %>% 
    summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
    filter(SignalPow > 0)
  
  return(TrainDF %>% 
           subset(WAP %in% GoodWAPs$WAP))
}


#str(Wifi0) #21048

# str(
# Wifi0 %>% unique() #20411
# )
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
 
  


#str(WifiMelted) #10613720

colnames(TempWifiMelted)[12] <- "WAP"
colnames(TempWifiMelted)[13] <- "SignalPow"


#LETS define a sequence to use as primary key:
TempWifiMelted$KEY <- 1:nrow(TempWifiMelted) 
#10613644
WifiMelted <- RemoveBotheringWAPs(TempWifiMelted) # %>% select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID, WAP, SignalPow, KeySample, KEY)


#TestWifi$SignalPow = as.double(TestWifi$SignalPow)

