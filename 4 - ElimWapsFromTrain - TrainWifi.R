
# getting a training set with the relevant waps.

RemoveBotheringWAPs <- function(TrainDF) {
  
    GoodWAPs <- TrainDF %>% group_by(
    WAP) %>% 
    summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
    filter(SignalPow > 0)
    
    return(TrainDF %>% 
           subset(WAP %in% GoodWAPs$WAP))
}

# 
#### eliminating useless WAPs ####
#defining the waps employed in Test.
TempDF <- WifiMelted %>% filter(TEST == TRUE ) %>% 
  filter(SignalPow != 0) %>% dplyr::select(WAP)



#Creating a trainning set with only the relevant Waps:
TrainWifi <- RemoveBotheringWAPs(WifiMelted  %>% filter(TEST == FALSE )   #lets do it more easily
      ) %>% subset(WAP %in% TempDF$WAP)  %>% 
  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                                  #, USERID
                                                  #, PHONEID
                                                  #timestamp...
                           , WAP
                                                  ,SignalPow)  



TestWifi <- WifiMelted  %>% filter(TEST == TRUE ) %>% 
  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                , USERID
                , PHONEID
                ,TIMESTAMP
                , WAP
                ,SignalPow) %>% # unique()
  tidyr::spread(WAP, SignalPow, convert = FALSE) 
  


TrainWifiAgg <- TrainWifi %>% group_by(#LONGITUDE,LATITUDE,
                                       FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
                                       #, USERID
                                       #, PHONEID
                                       #timestamp...
                                       , WAP) %>%
 sample_n(1000000) summarize(SignalPow = as.numeric(median(SignalPow))) %>% ungroup()


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

