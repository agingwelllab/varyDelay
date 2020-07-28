# Graph temp discounting results
# 4.15.20 KLS and SL

# load required packages
library(here)
library(plyr)
library(ggplot2)
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

# restructure data for analysis 
d0 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005)
rm(gd)

d0$delay <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,1])
d0$kval <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,2])

d0$delay_unit <- ifelse(str_detect(d0$delay, 'd'), 'days', 
                        ifelse(str_detect(d0$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d0$delay, 'm'), 'months',
                                      ifelse(str_detect(d0$delay, 'y'), 'years', 0))))

# recode choice into LL (0) or SS (1)
d0$choice <- ifelse(d0$choice == 2, 0, 1)
d0$choice <- as.numeric(d0$choice)

# graph of averages across delay unit and kvalue
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)

d1$delay_unit <- factor(d1$delay_unit, levels = c("days", "weeks", "months", "years"))

graph_delay_unit_kval <- ggplot(d1, aes(delay_unit, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay')

graph_delay_unit_kval

### testing SL- trying to graph main effect of delay unit
graph_delay_unit <- ggplot(d1, aes(delay_unit, choice)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay')

graph_delay_unit
###testing over

# by age
d0$agegrp <- ifelse(d0$Age > median(d0$Age), 'Older', 'Younger')

# graph of averages across delay unit and kvalue
d2 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp","delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
d2$delay_unit <- factor(d2$delay_unit, levels = c("days", "weeks", "months", "years"))

# graph_delay_unit_kval_age <- ggplot(d2, aes(delay_unit, choice, fill = kval)) + 
#   geom_bar(stat='identity', position=position_dodge()) +
#   geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
#   theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay') + facet_wrap(.~agegrp)

graph_delay_unit_kval_age <- ggplot(d2, aes(kval, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('kval') + facet_grid(delay_unit~agegrp) + theme(legend.position = 'null')

d3 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp","delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
d3$delay_unit <- ordered(d3$delay_unit, levels = c("days", "weeks", "months", "years"))
graph_delay_unit_age <- ggplot(d3, aes(delay_unit, choice, color = agegrp)) + geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.1) +
  geom_line() + geom_point() + geom_line() +theme_minimal() + ylab('Proportion SS Choice') 
  
graph_delay_unit_age


