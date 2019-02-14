
# getting a training set with the relevant waps.

RemoveBotheringWAPs <- function(TrainDF) {
  
    GoodWAPs <- TrainDF %>% group_by(
    WAP) %>% 
    summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
    filter(SignalPow > 0)
    
    return(TrainDF %>% 
           subset(WAP %in% GoodWAPs$WAP))
}





#### eliminating useless WAPs ####
#defining the waps with signal > 0
TempDF <- WifiMelted %>% dplyr::filter(TEST == TRUE ) %>% 
  filter(SignalPow != 0) %>% dplyr::select(WAP)



TrainWifi <- RemoveBotheringWAPs(
  WifiMelted %>% subset(WAP %in% TempDF$WAP) %>% 
  group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
             #, USERID
             #, PHONEID
             #timestamp...
             , WAP) %>%
  sample_n(1) %>% ungroup()) %>% select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID, WAP, SignalPow, KEY)

# 


TestWifiTemp <- WifiMelted %>%
subset(!KEY %in% TrainWifi$KEY) %>% 
  tidyr::spread(WAP, SignalPow, convert = FALSE) 


TestWifi <- TestWifiTemp %>% select(LONGITUDE, LATITUDE, FLOOR, BUILDINGID, TEST, WAP, SignalPow, KEY) %>% 
  tidyr::spread(WAP, SignalPow, convert = FALSE) 






TrainWifiAgg <- TrainWifi %>% group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION 
                                       #, USERID
                                       #, PHONEID
                                       #timestamp...
                                       , WAP) %>%
  summarize(SignalPow = as.numeric(median(SignalPow))) %>% ungroup()



# #old way (no function)
# 
# GoodWAPs <- Wifi4Visual %>% filter(TEST == FALSE) %>% group_by(
#   #, USERID
#   #, PHONEID
#   #timestamp...
#   #,
#   WAP) %>%
#   summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
#   filter(SignalPow > 0)
# 
# #%>% unique()
# TrainWifiTest <- WifiMelted  %>% filter(TEST == FALSE ) %>%
#   subset(WAP %in% TempDF$WAP) %>%
#   subset(WAP %in% GoodWAPs$WAP)  %>% 
#   dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                 #, USERID
#                 #, PHONEID
#                 #timestamp...
#                 , WAP
#                 ,SignalPow) %>% 
#   mutate( SignalPow = case_when(SignalPow > 75 & SignalPow < 200  
#                                 ~ SignalPow - 40,
#                                 TRUE ~ SignalPow) 
#   )
# 
# str(TrainWifi)
# str(TrainWifiTest)
#same should be done for Test Set....

