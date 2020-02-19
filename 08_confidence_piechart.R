#pie chart of confidence
#SL 2.18.20

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#data key(Qualtrics): 1= certain; 2= probably; 3= probably not; 4= certainly not

#extract confidence column from data
confidence <- dt$confidence

#make into confidence frame- still has NA values
conFrame <- as.data.frame(confidence)

#get frequency table of conFrame
conFreq <- as.data.frame(table(conFrame))

#make conFreq into piechart
title <- "How confident are you to unexpectedly come up with $2,000 in 30 days?"
x <-conFreq$Freq
y <- conFreq$conFrame
#y <- c("certainly could", "probably could", "certainly not")
pie(x,y, main=title)

#issue is that there is no "3/probably" in data sample!
# so worried to hardcode 1,2,4 without 3!
#if all four levels: y <- c("certainly could", "probably could", "probably not", "certainly not")