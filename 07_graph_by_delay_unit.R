# Graph temp discounting results
# 4.15.20 KLS and SL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(ggplot2)
library(ggdist)
library(EnvStats)

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

# Add Age group
d0$agegrp <- ifelse(d0$Age > median(d0$Age), 'Older', 'Younger')

# graph of averages across delay unit and kvalue (Now breaks down by age group also.)
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp", "delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)

d1$delay_unit <- factor(d1$delay_unit, levels = c("days", "weeks", "months", "years"))

graph_delay_unit_kval <- ggplot(d1, aes(delay_unit, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay') +
  facet_grid(rows=vars(agegrp)) + 
  scale_fill_hue(name = "Discount Rate") + scale_color_grey(name = "Discount Rate") 

graph_delay_unit_kval

# graph of averages across delay unit and kvalue
d2 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp","delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
d2$delay_unit <- factor(d2$delay_unit, levels = c("days", "weeks", "months", "years"))

graph_delay_unit_kval_age <- ggplot(d2, aes(kval, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('kval') + facet_grid(delay_unit~agegrp) + theme(legend.position = 'null')

# graph of averages across delay unit by age - FIGURE 1
d3 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp","delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
d3$delay_unit <- ordered(d3$delay_unit, levels = c("days", "weeks", "months", "years"))

graph_delay_unit_age <- ggplot(d3, aes(delay_unit, choice, fill = agegrp)) + 
  geom_bar(stat= 'identity', position=position_dodge()) + 
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) +
  theme_minimal() + theme(plot.title = element_text(face="bold", size = 24),
                            axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
                            axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
                            strip.text.x = element_text(size=20), 
                          legend.text = element_text(size = 16), legend.title = element_text(size = 20), legend.position = 'top') + 
  scale_fill_discrete(name = "Age Group") + scale_color_discrete(name = "Age Group") + 
  ylab('Proportion SS Choice') + xlab('Delay Unit') 
  
graph_delay_unit_age

# Raincloud graphs
d2$delay_unit <- ordered(d2$delay_unit, levels = c("days", "weeks", "months", "years"))
ggplot(d2, aes(x = delay_unit, y = choice, fill = agegrp)) + 
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95)
  )  +
  coord_cartesian(xlim = c(1.2, NA)) +
  facet_grid(rows = vars(agegrp)) +
  coord_flip() +
  theme_minimal() + theme(plot.title = element_text(face="bold", size = 24),
                          axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
                          axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size=20), 
                          legend.text = element_text(size = 16), legend.title = element_text(size = 20), legend.position = 'top') + 
  scale_fill_hue(name = "Age Group") + scale_color_grey(name = "Age Group") + 
  ylab('Proportion of Smaller Sooner Choices') + xlab('Delay Unit') 

# Stacked Graph
ggplot(d2, aes(x = delay_unit, y = choice, fill = agegrp)) + 
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6, 
    ## set slab interval to show IQR and 95% data range
    .width = c(.5, .95)
  )  +
  coord_cartesian(xlim = c(1.2, NA)) +
  coord_flip() +
  theme_minimal() + theme(plot.title = element_text(face="bold", size = 24),
                          axis.title.x = element_text(size = 24), axis.title.y = element_text(size = 24),
                          axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
                          strip.text.x = element_text(size=20), 
                          legend.text = element_text(size = 16), legend.title = element_text(size = 20), legend.position = 'top') + 
  scale_fill_hue(name = "Age Group") + scale_color_grey(name = "Age Group") + 
  ylab('Proportion of Smaller Sooner Choices') + xlab('Delay Unit') 

#save graphs
png(here::here('figs', 'delay_unit_x_age_grp.png'), width = 600, height = 600)
graph_delay_unit_age
dev.off()

