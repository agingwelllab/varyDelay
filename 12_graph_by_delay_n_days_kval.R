# 8.2.21 KLS
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(plyr)
library(tidyverse)

# load source functions
source(here::here('scr', 'transform_delay_and_k.R'))
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days ####

d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

d1 <- create_delay_unit(d1)
d1 <- create_delay_n_days(d1)
d1 <- create_k_value(d1)

# recode choice into LL (0) or SS (1)
d1$choice <- ifelse(d1$choice == 2, 0, 1)
d1$choice <- as.numeric(d1$choice)

# make delay_n_days factor
d1$delay_n_days <- factor(d1$delay_n_days)

## Supplement Fig  graph choice by age by delay and kval ####
choice_by_age_by_delay_n_days_kval <- ggplot(d1, aes(Age, choice, color = delay_n_days, fill = delay_n_days)) + 
  geom_smooth(method = 'lm') + theme_minimal() + 
  theme_minimal() + theme(plot.title = element_text(face="bold", size = 24),
                          axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
                          axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size=20),
                          legend.text = element_text(size = 16), legend.title = element_text(size = 20), legend.position = 'top') + 
  scale_color_discrete(name = 'Delay in Days') + scale_fill_discrete(name = 'Delay in Days') + 
  ylab('Proportion of SS Choices') + facet_wrap(kval ~delay_unit)

choice_by_age_by_delay_n_days_kval

#save graph
png(here::here('figs', 'delay_in_days_x_age_kval.png'), width = 600, height = 600)
choice_by_age_by_delay_n_days_kval
dev.off()

