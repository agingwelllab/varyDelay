#pie chart of health insurance
#SL 2.19.20

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#data key (Qualtrics): 1= yes, 2= No, NA=NA; sample only has 1s

#extract health_ins column from data
health_ins <- dt$health_ins

#make into health_ins frame- still has NA values
hsFrame <- as.data.frame(health_ins)

#get frequency table of hsFrame
hsFreq <- as.data.frame(table(hsFrame))

#make hsFreq into piechart
title <- "Do you currenly have health insurance?"
x <-hsFreq$Freq
#y <- hsFreq$hsFrame
y <- c("yes")
pie(x,y, main=title)
