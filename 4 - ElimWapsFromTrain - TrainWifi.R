
# getting a training set with the relevant waps.

GoodWAPs <- Wifi4Visual %>% filter(TEST == FALSE) %>% group_by(
  #, USERID
  #, PHONEID
  #timestamp...
  #,
  WAP) %>% 
  summarize(SignalPow = as.numeric(sum(SignalPow))) %>%
  filter(SignalPow > 0)

#### eliminating useless WAPs ####
#defining the waps employed in Test.
TempDF <- WifiMelted %>% filter(TEST == TRUE ) %>% 
  filter(SignalPow != 0) %>% dplyr::select(WAP)

#Creating a trainning set with only the relevant waps:
TrainWifi <- WifiMelted  %>% filter(TEST == FALSE ) %>%
  subset(WAP %in% TempDF$WAP)  %>% 
  subset(WAP %in% GoodWAPs$WAP) %>% 
  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
                                                  #, USERID
                                                  #, PHONEID
                                                  #timestamp...
                                                  , WAP
                                                  ,SignalPow) %>% 
                                                 mutate( value = case_when(SignalPow > 75 & SignalPow < 200  
                                                                           ~ SignalPow - 40,
                                                  TRUE ~ SignalPow) 
                                                  )
#%>% unique()


#same should be done for Test Set....
