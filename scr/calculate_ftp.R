calculate_ftp <- function(data, id) {
  ### This function takes the Future Time Perspective data and calculates a 
  ### summary score
  ### data: the full data frame
  ### id: the column number (or numbers) for participant identifier(s)
  ### cols: columns of FTP data 
  library(here)
  library(psych)
  source(here('scr', 'isolate_data.R'))
  d0 <- isolate_data(data, id, grep('FTP', colnames(dt))[1]:rev(grep('FTP', colnames(dt)))[1])
  keys<-c(1,1,1,1,1,1,1,-1,-1,-1)
  items <- d0[2:11]
  d1 <- reverse.code(keys, items, mini = 1, maxi=7)
  d2 <- cbind.data.frame(d0[1], rowMeans(d1))
  colnames(d2) <- c('ID', 'FTP')
  return(d2)
}

