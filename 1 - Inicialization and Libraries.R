


#strsplit(version[['version.string']], ' ')[[1]][3]


#install.packages("Rserve")
library(esquisse)



#### Load Libraries ####
library(pacman)
pacman::p_load(readr, caret, plotly, ggplot2, 
               labeling, promises, ggridges, 
               doParallel,  e1071, mlbench,# inum,
               corrplot#, ggpubr
               , rpart, rpart.plot, gbm
               , boot, dplyr,
               reshape, Rserve#, tidyvers
               , padr)

#library(proj4)

#### Enable parallel processing ####
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
on.exit(stopCluster(cl))