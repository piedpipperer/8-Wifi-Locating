
# getting a training set with the relevant waps.

GoodWAPs <- WifiMelted %>% filter(TEST == FALSE) %>% group_by(
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
  subset(WAP %in% GoodWAPs$WAP) 
#same should be done for Test Set....