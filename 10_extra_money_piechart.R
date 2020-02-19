#pie chart of extra_money
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

#data key (qualtrics): 1= yes- have to repay; 2- yes- not have to repay; NA=NA

#extract money column from data
money <- dt$extra_money

#make into money frame- still has NA values
moneyFrame <-as.data.frame(money)

#get frequency table of moneyFrame
moneyFreq <-as.data.frame(table(moneyFrame))

#make investFrame into piechart
title <- "Would friends/family loan money and expect to be repaid?"
x <- moneyFreq$Freq
y <- c( "Yes- have to repay", "No- not have to repay")
pie(x,y, main=title)

#only concern is it removed NA!!
#need to figure out how to keep NA in as an option!
#replace(investments, "NA", 0) #THIS DOES NOT WORK!!