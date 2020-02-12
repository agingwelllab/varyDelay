#histogram of age
#SL 12.19.09

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#extract age from data
Age <- dt$Age

#calculate number of bins needed
bins = seq(min(Age, na.rm=TRUE), max(Age, na.rm=TRUE), 1)

#build histogram
hist(Age, breaks= bins, col= "grey")
