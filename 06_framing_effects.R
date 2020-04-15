# 2.26.20 KLS, CRG
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

# Framing effects analysis

# pull out 7d, 1w, 4w, 1m, 12m, 1y columns
d2 <- gd[,c(1, 2, 9:14, 18:23, 27:32)] 
d2 <- gather(d2, "gambletype", "choice", X7d_1:X1y_1)

# Recode trial based on frame; 1 = small number, large unit (e.g. 1 week), 
# 0 = large number, small unit (e.g. 7 days)
d2$frame <- ifelse(str_detect(d2$gambletype, 'X1'), 1, 0) 

# break apart gamble type into delay and k val columns
d2$delay <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,1])
d2$kval <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,2])

# convert the letter in delay variable to label
d2$delay_unit <- ifelse(str_detect(d2$delay, 'd'), 'days', 
                        ifelse(str_detect(d2$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d2$delay, 'm'), 'months',
                                      ifelse(str_detect(d2$delay, 'y'), 'years', 0))))

# convert k vals to numeric
d2$kval <- paste0('.', d2$kval) # more concise
d2$kval <- as.numeric(as.character(d2$kval))

# recode choice into accept (1) or reject (0)
d2$choice <- d2$choice - 1 # more concise
d2$choice <- as.numeric(d2$choice)

# Create Models and Test
M3 <- glm(d2$choice~d2$frame+d2$delay_unit+d2$Age+d2$kval, family=binomial(link='logit'), data = d2)


