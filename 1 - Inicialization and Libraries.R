


#strsplit(version[['version.string']], ' ')[[1]][3]


#### Load Libraries ####
libraries_function <- function(){
  if(require("pacman")=="FALSE"){
    install.packages('pacman')
    library('pacman')
    pacman::p_load(anytime,tidyverse,lubridate,plotly,cowplot,reshape2,expss,lattice,ggplot2,rpart,esquisse,xgboost
                   ,randomForest,liquidSVM,DMwR,kknn,C50,e1071,doParallel,zoo)
  } else {
    library('pacman')
    pacman::p_load(anytime,tidyverse,lubridate,plotly,cowplot,reshape2,expss,lattice,ggplot2,rpart,xgboost
                   ,randomForest,liquidSVM,DMwR,kknn,C50,e1071,doParallel,zoo)
  }
  
}



