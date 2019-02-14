

#str(Wifi0) #21048

# str(
# Wifi0 %>% unique() #20411
# )

WifiMelted <- Wifi0 %>% unique() %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                         ,"RELATIVEPOSITION","USERID","PHONEID",
                                         "TIMESTAMP", "TEST")
)  %>% 
  mutate( value = value + 105) %>% 
  mutate( value = case_when(value == 205 ~ 0,
                            value > 75 & value < 200 ~ value - 40,
                            TRUE ~ value) 
  )  #filter( value != 100) %>% 
 
  


#str(WifiMelted) #10613720

colnames(WifiMelted)[11] <- "WAP"
colnames(WifiMelted)[12] <- "SignalPow"


#LETS define a sequence to use as primary key:
WifiMelted$KEY <- 1:nrow(WifiMelted) 
#10613644


#TestWifi$SignalPow = as.double(TestWifi$SignalPow)

