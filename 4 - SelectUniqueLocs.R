

TakeNumberUniqueLocations <- function(WifiUnMelt, N) {
  
 # if(N == NULL) { N=1}
  TrainWifiAllLocs <- WifiUnMelt %>% dplyr::group_by(LONGITUDE,LATITUDE,FLOOR
                                                     ,BUILDINGID
                                                     ,#, WAP, SignalPow
                                                     #,
                                                     #KeySample
  ) 
  
  
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



