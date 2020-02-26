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

# restructure data for analysis 
d0 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005)
rm(gd)

d0$delay <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,1])
d0$kval <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,2])

d0$delay_unit <- ifelse(str_detect(d0$delay, 'd'), 'days', 
       ifelse(str_detect(d0$delay, 'w'), 'weeks', 
              ifelse(str_detect(d0$delay, 'm'), 'months',
                     ifelse(str_detect(d0$delay, 'y'), 'years', 0))))

## Center age
d0$Age <- scale(d0$Age, center = TRUE, scale=TRUE)

# choice 1 = SS, choice 2 = LL

# 3 (kval) x 4 (delay_unit) x Age Within-Subjects ANCOVA
m1 <- ezANOVA(data = d0, dv = .(choice), wid = .(ID), within = .(delay_unit, kval), between = .(Age))

# create new variable - delay_n_days

# 3 (kval) x 10 (delay_n_days) x Age Within-Subjects ANCOVA
M2 <- m1 <- ezANOVA(data = d0, dv = .(choice), wid = .(ID), within = .(delay_n_days, kval), between = .(Age))


