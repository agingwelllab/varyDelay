#pie chart of race
#SL 12.19.19

# load required packages
library(plyr)
library(here)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#extract Race
Race <- dt$Race

#make Race categorical (factor)
as.factor(Race)

#get Race Frequencies & save as its own table
Race_frequency <- count(Race)
Race_frequency <- Race_frequency [c(1:3),]
Race_frequency <- count(Race)

#rename to race & frequency
names(Race_frequency) [1] = "Race"

# create barchart of Race & frequency
ggplot(Race_frequency, aes(x="", y= freq, fill= Race)) + geom_bar (stat= "identity", width=1)

#make it circular with coord_polar()
ggplot(Race_frequency, aes(x="", y= freq, fill= Race)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0)

