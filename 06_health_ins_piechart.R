#pie chart of health insurance
#SL 2.11.20

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#extract health insurance from data
health_ins <- dt$health_ins

#make insurance categorical (factor)
as.factor(health_ins)

#get health_ins Frequencies & save as its own table
health_ins_frequency <- count(health_ins)
health_ins_frequency <- health_ins_frequency [c(1:3),]
health_ins_frequency <- count(health_ins)

#rename to health_ins & frequency
names(health_ins_frequency) [1] = "Health_Ins"

# create barchart of health_ins & frequency- getting stuck here!!!!!!
ggplot(health_ins_frequency, aes(x="", y= freq, fill= health_ins)) + geom_bar (stat= "identity", width=1)

#make it circular with coord_polar()
ggplot(health_ins_frequency, aes(x="", y= freq, fill= health_ins)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0)
