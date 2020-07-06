# 6.23.20 KLS, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(plyr)
library(tidyverse)

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days

d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

# break apart gamble type into delay and k val columns
d1$delay <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,1]) #create delay column
d1$kval <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,2]) # create k val column

# convert the letter in delay variable to # of days (d for day = 1, w for week = 7, m for month = 30, 
# y for year = 365)
d1$delay_n_days <- ifelse(str_detect(d1$delay, 'd'), '1', 
                          ifelse(str_detect(d1$delay, 'w'), '7', 
                                 ifelse(str_detect(d1$delay, 'm'), '30',
                                        ifelse(str_detect(d1$delay, 'y'), '365', 0))))

# isolate the number from delay variable 
d1$delay <- str_remove_all(d1$delay, "[Xdwmy]")
#d1$gambletype <- NULL # remove gamble type variable

d1$delay <- as.numeric(d1$delay)
d1$delay_n_days <- as.numeric(d1$delay_n_days)
d1$delay_n_days <- d1$delay*d1$delay_n_days
d1$delay_n_days <- as.numeric(str_replace(d1$delay_n_days, "28", "30")) 
d1$delay_n_days <- as.numeric(str_replace(d1$delay_n_days, "360", "365"))

# convert k vals to numeric
d1$kval <- paste0('.', d1$kval) # more concise
d1$kval <- as.numeric(as.character(d1$kval))

# recode choice into LL (0) or SS (1)
d1$choice <- ifelse(d1$choice == 2, 0, 1)
d1$choice <- as.numeric(d1$choice)

# make delay_n_days factor
d1$delay_n_days <- factor(d1$delay_n_days)

## graph choice by age by delay
choice_by_age_by_delay_n_days <- ggplot(d1, aes(Age, choice, color = delay_n_days, fill = delay_n_days)) + 
  geom_smooth(method = 'lm', se = FALSE) + theme_minimal() + theme(legend.position="top") 

choice_by_age_by_delay_n_days
