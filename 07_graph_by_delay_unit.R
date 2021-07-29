# Graph temp discounting results
# 4.15.20 KLS and SL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(ggplot2)

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

# restructure data for graphing ####
d0 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005)
rm(gd)

d0$delay <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,1])
d0$kval <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,2])

d0$delay_unit <- ifelse(str_detect(d0$delay, 'd'), 'days', 
                        ifelse(str_detect(d0$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d0$delay, 'm'), 'months',
                                      ifelse(str_detect(d0$delay, 'y'), 'years', 0))))
d0$delay_unit <- factor(d0$delay_unit, levels = c("days", "weeks", "months", "years"))

# recode choice into LL (0) or SS (1)
d0$choice <- ifelse(d0$choice == 2, 0, 1)
d0$choice <- as.numeric(d0$choice)

# Add Age group
d0$agegrp <- ifelse(d0$Age > median(d0$Age), 'Older', 'Younger')

# Figure 1 ####
fig1 <- ggplot(d0, aes(Age, choice, fill = delay_unit, colour = delay_unit)) + 
  geom_smooth(method = lm) + theme_bw() + ylab('Proportion SS Choice') + xlab('Age') + 
  scale_fill_discrete(name="Delay Condition") + scale_colour_discrete(name="Delay Condition") + 
  theme(plot.title = element_text(face="bold", size = 24),axis.title.x = element_text(size = 24), 
        axis.title.y = element_text(size = 24), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
        strip.text.x = element_text(size=20), legend.text = element_text(size = 16), legend.title = element_text(size = 20), 
        legend.position = 'top')

png(here::here('figs', 'propSS_x_age_x_delay_unit.png'), width = 600, height = 600)
fig1
dev.off()


# Figure S1 - graph of averages across delay unit and kvalue (Now breaks down by age group also) ####
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp", "delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
graph_delay_unit_kval <- ggplot(d1, aes(delay_unit, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay') +
  facet_grid(rows=vars(agegrp)) + 
  scale_fill_hue(name = "Discount Rate") + scale_color_grey(name = "Discount Rate") 

graph_delay_unit_kval


# Figure S2 - Stacked Graph of distributions ####
d2 <- summarySE(data=d0, measurevar="choice", groupvars=c("agegrp","delay_unit","kval"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
stacked <- ggplot(d2, aes(x = delay_unit, y = choice, fill = agegrp, alpha = 0.5)) + 
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

png(here::here('figs', 'stacked_distribution_propSS_x_age_grp.png'), width = 600, height = 600)
stacked
dev.off()

# main effect of delay unit ####
d4 <- summarySE(data=d0, measurevar="choice", groupvars=c("delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
graph_delay_unit <- ggplot(d4, aes(delay_unit, choice, fill = delay_unit)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Unit of Delay') + theme(legend.position = "None")
graph_delay_unit

png(here::here('figs', 'main_effect_of_delay_unit.png'), width = 600, height = 600)
graph_delay_unit
dev.off()
