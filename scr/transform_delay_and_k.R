# helper functions to transform gamble type variable into delay and k values for data analysis

create_delay_unit <- function(dt) {
  # break apart gamble type into delay and k val columns
  dt$delay <- as.factor(t(as.data.frame(strsplit(dt$gambletype, '_')))[,1]) #create delay column
  dt$delay_unit <- ifelse(str_detect(dt$delay, 'd'), 'days', 
                          ifelse(str_detect(dt$delay, 'w'), 'weeks', 
                                 ifelse(str_detect(dt$delay, 'm'), 'months',
                                        ifelse(str_detect(dt$delay, 'y'), 'years', 0))))
  dt$delay_unit <- factor(dt$delay_unit, levels = c("days", "weeks", "months", "years"))
  dt$delay <- NULL
  return(dt)
}

create_delay_n_days <- function(dt) {
  dt$delay <- as.factor(t(as.data.frame(strsplit(dt$gambletype, '_')))[,1]) #create delay column
  dt$delay_n_days <- ifelse(str_detect(dt$delay, 'd'), '1', 
                            ifelse(str_detect(dt$delay, 'w'), '7', 
                                   ifelse(str_detect(dt$delay, 'm'), '30',
                                          ifelse(str_detect(dt$delay, 'y'), '365', 0))))
  # isolate the number from delay variable 
  dt$number <- str_remove_all(dt$delay, "[Xdwmy]")
  
  dt$number <- as.numeric(dt$number)
  dt$delay_n_days <- as.numeric(dt$delay_n_days)
  dt$delay_n_days <- dt$number*dt$delay_n_days
  dt$delay_n_days <- as.numeric(str_replace(dt$delay_n_days, "28", "30")) 
  dt$delay_n_days <- as.numeric(str_replace(dt$delay_n_days, "360", "365"))
  
  # make delay_n_days factor
  dt$delay_n_days <- factor(dt$delay_n_days)
  dt$delay <- NULL; dt$number <- NULL
  return(dt)
}

create_k_value <- function(dt){
  dt$kval <- as.factor(t(as.data.frame(strsplit(dt$gambletype, '_')))[,2]) # create k val column
  # convert k vals to numeric
  dt$kval <- paste0('.', dt$kval) # more concise
  dt$kval <- as.numeric(as.character(dt$kval))
  return(dt)
}



