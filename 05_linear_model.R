# 1.29.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(plyr)
library(ggplot2)
library(tidyverse)
library(ez)

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data
gd <- isolate_data(dt, c(1,8), 32:67)
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days

d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) 

d1$delay <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,1])
d1$kval <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,2])

d1$delay_n_days <- ifelse(str_detect(d1$delay, 'd'), '1', 
                          ifelse(str_detect(d1$delay, 'w'), '7', 
                                 ifelse(str_detect(d1$delay, 'm'), '30',
                                        ifelse(str_detect(d1$delay, 'y'), '365', 0))))

d1$delaym <- str_remove_all(d1$delay, "[Xdwmy]")
d1$delayND <- as.numeric(d1$delay_n_days)
d1$delayM <- as.numeric(d1$delaym)
d1$choice <- as.numeric(d1$choice)
d1$delay_n_days <- d1$delayM*d1$delayND
d1 <- d1[, -which(names(d1) %in% c("delaym", "delayND", "delayM", "DelayDays", "gambletype", "delay"))]
d1$delay_n_days <- str_replace(d1$delay_n_days, "28", "30") 
d1$delay_n_days <-str_replace(d1$delay_n_days, "360", "365")
d1$choice <- str_replace(d1$choice, "1", "0")
d1$choice <- str_replace(d1$choice, "2", "1")
d1$choice <- as.numeric(d1$choice)
d1$delay_n_days <- as.numeric(d1$delay_n_days)

# Simple Logistic Regression

M2 <- glm(d1$choice~d1$Age+d1$kval+d1$delay_n_days,family=binomial(link='logit'), data = d1)

# 7dvs1w Model

d2 <- gd[,c(1, 2, 9:14, 18:23, 27:32)]
d2 <- gather(d2, "gambletype", "choice", X7d_1:X1y_1) 

d2$delay <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,1])
d2$kval <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,2])

d2$delay_unit <- ifelse(str_detect(d2$delay, 'd'), 'days', 
                        ifelse(str_detect(d2$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d2$delay, 'm'), 'months',
                                      ifelse(str_detect(d2$delay, 'y'), 'years', 0))))
d2$delay <- str_remove_all(d2$delay, "[Xdwmy]")
d2 <- as.data.frame(t(d2))

