
retrieve_last_file <- function(directory, key_word = "validationData") {
  
  #setwd(directory)
  #directory <- paste(getwd(),"/csv/",sep="")
  
  df <- file.info(
    list.files(directory, pattern = paste(key_word) )
  )
  
  dataset <- read.csv(paste(directory, row.names(df %>% sample_n(1)),sep=""), header = T, sep = ",")
  
  return(dataset)
  
}

