calculate_ftp <- function(data) {
  ### This function takes the Future Time Perspective data and calculates a 
  ### summary score
  ### data: the full data frame
  library(here)
  library(psych)
  source(here::here('scr', 'isolate_data.R'))
  d0 <- isolate_data(data, grep('ID', colnames(data)), grep('FTP', colnames(dt)))
  if("PID" %in% colnames(d0)) {
    d0$PID <- NULL
  }
  keys<-c(1,1,1,1,1,1,1,-1,-1,-1)
  items <- d0[2:11]
  d1 <- reverse.code(keys, items, mini = 1, maxi=7)
  d2 <- cbind.data.frame(d0[1], rowMeans(d1))
  colnames(d2) <- c('ID', 'FTP')
  return(d2)
}

