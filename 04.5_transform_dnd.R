# data transformations KLS
# 7.30.21 CRG, KLS
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here::here('scr', 'transform_delay_and_k.R'))
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# load model 2
M2 <- readRDS(here::here('output', 'model2.RDS'))

# isolate gamble data ####
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days
d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

d1 <- create_delay_n_days(d1)
d1 <- create_k_value(d1)

# transform delay_n_days variable ####
hist(d1$delay_n_days)

d1$sqrtdd <- sqrt(d1$delay_n_days)
hist(d1$sqrtdd)

d1$logdnd <- log(d1$delay_n_days)
hist(d1$logdnd)
#logdnd wins!