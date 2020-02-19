#pie chart of investments
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

#data key (qualtrics): 1= Yes, 2= No, NA=NA

#extract investment column from data
investments <- dt$investments

#make into investment frame- still has NA values
investFrame <-as.data.frame(investments)

#get frequency table of investFrame
investFreq <-as.data.frame(table(investFrame))

#make investFrame into piechart
title <- "Do you currently have non-retirement investments?"
x <- investFreq$Freq
y <- c( "Yes", "No")
pie(x,y, main=title)

#only concern is it removed NA!!
#need to figure out how to keep NA in as an option!
#if you make data table b4 tabling, it keeps NA!
#replace(investments, "NA", 0) #THIS DOES NOT WORK!!