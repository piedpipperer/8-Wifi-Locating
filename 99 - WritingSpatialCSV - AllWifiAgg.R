
#### Convert of AggInfo into grades ####
#lets try an agg version of the data: 

# TestWifiAgg <- TestWifi %>%  group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                        #, USERID
#                                        , PHONEID
#                                        #timestamp...
#                                        , WAP) %>% 
#   summarize(SignalPow = mean(SignalPow))
# 
# TrainWifiAgg2 <- TrainWifi2 %>% group_by(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                         #, USERID
#                                         , PHONEID
#                                         #timestamp...
#                                         , WAP) %>% 
#   summarize(SignalPow = median(SignalPow)) %>% filter (SignalPow != 0)





AllWifiCoord <- WifiMelted %>%  dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID#,RELATIVEPOSITION
                                              #, USERID
                                              , PHONEID
                                              , TEST
                                              #timestamp...
                                              , WAP
                                              , SignalPow) %>% filter (SignalPow != 0)



#AllWifiAgg$TRAINorTEST <- AllWifiAgg$TEST


library(rgdal)
library(raster)

proj4string <- "+proj=utm +zone=31t +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

proj4stringT <- "+proj=longlat +zone=31t +north  "

AllWifiCoord$FLOOR <- as.numeric(AllWifiCoord$FLOOR) * 10

# TEST <- TrainWifi %>% select(LONGITUDE,LATITUDE)
# 
# colnames(TEST)[1] <- "x"
# colnames(TEST)[2] <- "y"
# str(TEST)
# 
# pj <- rgdal::project(TEST, proj4string#, inverse=TRUE
#               )


#str(TrainWifi2)
# Look4Out <- TrainWifi2 %>% dplyr::select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION
#                                          , USERID
#                                          , PHONEID, SignalPow
#                                          #timestamp...
#                                          , WAP) %>% filter (SignalPow != 0) %>% 
#   filter (SignalPow > 30)

# Look4Out <- TrainWifi2 %>% filter (SignalPow > 30)

# Look4Out$TIMESTAMP <- NULL
# Look4Out$USERID <- NULL
# Look4Out$RELATIVEPOSITION <- NULL



coordinates(AllWifiCoord) <- c("LONGITUDE", "LATITUDE")

proj4string(AllWifiCoord) <- CRS(proj4string)




# 
# print(
# project(TrainWifi,proj4string)
# )

# select the appropiate dataframe to store..

#l limimted to 7 features? besides coordinates.
Utm <- spTransform(AllWifiCoord, proj4stringT)
# Utm$LONGITUDE
# Utm$LATITUDE

Utm$optional <- NULL 

#esquisse::esquisser()

write.csv2(Utm,"../csv/testAndtrain2.csv") # this is for the kepler!!
write.csv(Utm,"../csv/testAndtrain.csv") # this is for the kepler!!

#write.csv2(Utm,"../csv/train2.csv")
# write.csv2(Utm4Out,"../csv/looking4Outliers2.csv")
# write.csv(Utm4Out,"../csv/looking4Outliers.csv")
