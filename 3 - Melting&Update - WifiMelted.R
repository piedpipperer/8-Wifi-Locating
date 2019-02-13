

#str(Wifi0) #21048

# str(
# Wifi0 %>% unique() #20411
# )

WifiMelted <- Wifi0  %>% melt( id.vars = c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID"
                                              ,"RELATIVEPOSITION","USERID","PHONEID",
                                              "TIMESTAMP", "TEST")
) %>% #filter( value != 100) %>% 
  mutate( value = value + 105) %>% 
  mutate( value = case_when(value == 205 ~ 0,
                            TRUE ~ value) 
  ) %>% unique()

#str(WifiMelted) #10613720

colnames(WifiMelted)[11] <- "WAP"
colnames(WifiMelted)[12] <- "SignalPow"



#TestWifi$SignalPow = as.double(TestWifi$SignalPow)

