

TakeNumberUniqueLocations <- function(WifiUnMelt, N) {
  
 # if(N == NULL) { N=1}
  TrainWifiAllLocs <- WifiUnMelt %>% dplyr::group_by(LONGITUDE,LATITUDE,FLOOR
                                                     ,BUILDINGID
                                                     ,#, WAP, SignalPow
                                                     #,
                                                     #KeySample
  )  %>% sample_n(1)
  
  #N<-10
  N <- N-1
  for (i in (c(1:(N)))
       )
  {
    TrainWifiAllLocs  <- rbind(TrainWifiAllLocs,
                               WifiUnMelt %>% subset(!KeySample %in% TrainWifiAllLocs$KeySample)
                               %>% dplyr::group_by(LONGITUDE,LATITUDE,FLOOR
                                                   ,BUILDINGID#, WAP, SignalPow
                                                   #,
                                                   #KeySample
                               )  %>% sample_n(1)
    )
  }
  
  return(TrainWifiAllLocs)
  
}
  

# getting a training set with the relevant waps.

BadWapss <- WifiMelted %>% filter(FALSE)   %>% dplyr::select(WAP) 
if (approach == "Minimal") {
  BadWapss <- read.csv("../csv/BadWaps.csv") #WAPS NOT used by the validation set.
  #GoodWaps <- 
}


WifiMelt2 <- WifiMelted  %>%
  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID, WAP, SignalPow, KeySample, TEST) %>% 
  subset(!WAP %in% BadWapss$WAP) 


WifiUnMelt <- WifiMelt2   %>% 
  tidyr::spread(WAP, SignalPow, convert = FALSE) 

#str(WifiUnMelt)

#### eliminating useless WAPs ####
#defining the waps with signal > 0
# TempDF <- WifiMelted %>% dplyr::filter(TEST == TRUE ) %>% 
#   filter(SignalPow != 0) %>% dplyr::select(WAP)

# making sure we will have at least every location!!


#1st iteration out of the loop, then loop!
# TrainWifiAllLocs <- 
#   TempWifiMelted %>% #subset(WAP %in% TempDF$WAP) %>% 
#   group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#              #, USERID
#              #, PHONEID
#              #timestamp...
#              #, KeySample
#            , WAP) %>%
#   sample_n(1) %>% ungroup()  
# 
#1st iteration out of the loop, then loop!

# TrainWifiAllLocs %>% dplyr::group_by(KeySample
# ) %>% summarise(n = n()) 

TrainWifiAllLocs <- TakeNumberUniqueLocations(WifiUnMelt,20)


 # TrainWifiAllLocs %>%   group_by (FLOOR, BUILDINGID) %>%  summarise(n = n()) 


# hem de canviar el loop!!



TestWifi <- WifiUnMelt %>%
    subset(!KeySample %in% TrainWifiAllLocs$KeySample) 
#3k1 






# antic bucle
# TempDF <- 
#   TempWifiMelted %>%  
#   subset(!KEY %in% TrainWifiAllLocs$KEY) %>%  #subset(WAP %in% TempDF$WAP)  %>%  
#   group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#            #, USERID
#            #, PHONEID
#            #timestamp...
#            , WAP) %>%
#   sample_n(1) %>% ungroup() 
# 
# TrainWifiAllLocs <- rbind(TrainWifiAllLocs,TempDF)
# TrainWifiAllLocsUnm <- rbind(TrainWifiAllLocsUnm,
#                              TempDF %>% dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,
#                                                       WAP, SignalPow,
#                                                       KeySample) %>% 
#                                tidyr::spread(WAP, SignalPow, convert = FALSE) 
# )

#TrainWifiAllLocs %>% filter(WAP == 'WAP506')

# tells how many samples can we get.
# WifiMelted %>%
#   subset(!KEY %in% TrainWifiAllLocs$KEY) %>% group_by (FLOOR, BUILDINGID,WAP) %>% 
#   summarise(n = n()) %>% arrange(n)

#9575138

##3,379,990
#
#write.csv2(TrainWifiAllLocs,"../csv/PruebaDup2.csv") 


#  trying to sample from building floor
# TempDF <- RemoveBotheringWAPs(
#   TempWifiMelted) %>%
# subset(!KEY %in% TrainWifiAllLocs$KEY) %>% #filter(WAP != 'WAP506') %>% 
#   group_by(#LONGITUDE,LATITUDE,
#            FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#            #, USERID
#            #, PHONEID
#            #timestamp...
#            , WAP) %>%
#   sample_n(2) %>% ungroup()




# too much memory, lets keep it simple momentarily
# TestWifi <- TestWifiTemp  %>% 
#   tidyr::spread(WAP, SignalPow, convert = FALSE) 






# 
# 
# TrainWifiAgg <- TrainWifi %>% group_by(#LONGITUDE,LATITUDE,
#   FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#   #, USERID
#   #, PHONEID
#   #timestamp...
#   , WAP) %>%
#   sample_n(1000000) summarize(SignalPow = as.numeric(median(SignalPow))) %>% ungroup()
# 



# #trying to do recursive function:
# TakeNDeepLocations <- function(TrainDF, N, TrainWifiAllLocs )
# {
#   if (N > 1 ){
#     TrainWifiAllLocsTMP <- TrainDF %>%
#       subset(!KEY %in% TrainWifiAllLocs$KEY) %>% #filter(WAP != 'WAP506') %>% 
#       group_by(LONGITUDE,LATITUDE,
#                FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#                #, USERID
#                #, PHONEID
#                #timestamp...
#                , WAP) %>%
#       sample_n(1)
#       N <- N - 1 
#       TrainWifiAllLocsTMP2 <- rbind(TrainWifiAllLocsTMP,TrainWifiAllLocs)
#       
#     return (TakeNDeepLocations(TrainDF, N, TrainWifiAllLocsTMP2))
#   }
#   else {
#     return (rbind( TempWifiMelted[FALSE,], RemoveBotheringWAPs(
#       TempWifiMelted) %>%
#       subset(!KEY %in% TrainWifiAllLocs$Keys) %>% #filter(WAP != 'WAP506') %>% 
#       group_by(LONGITUDE,LATITUDE,
#                FLOOR,BUILDINGID#,SPACEID,RELATIVEPOSITION 
#                #, USERID
#                #, PHONEID
#                #timestamp...
#                , WAP) %>%
#       sample_n(1))
#     )
#   }
#   
# }
# 
# TrainWifiAllLocs <- TakeNDeepLocations(RemoveBotheringWAPs(
#   TempWifiMelted),1,  TempWifiMelted %>% filter(FALSE))





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
# TrainWifiTest <- TempWifiMelted  %>% filter(TEST == FALSE ) %>%
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

