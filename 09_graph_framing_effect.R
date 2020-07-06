# Graph framing effect results
# 4.15.20 KLS, CRG
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

# Framing effects analysis

# pull out 7d, 1w, 4w, 1m, 12m, 1y columns
d2 <- gd[,c(1, 2, 9:14, 18:23, 27:32)] 
d2 <- gather(d2, "gambletype", "choice", X7d_1:X1y_1)

# Recode trial based on frame; 1 = small number, large unit (e.g. 1 week), 
# 0 = large number, small unit (e.g. 7 days)
d2$frame <- ifelse(str_detect(d2$gambletype, 'X1'), 1, 0) 
d2$frame <- ifelse(str_detect(d2$gambletype, 'X12'), 0, d2$frame)

# break apart gamble type into delay and k val columns
d2$delay <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,1])
d2$kval <- as.factor(t(as.data.frame(strsplit(d2$gambletype, '_')))[,2])

# convert the letter in delay variable to label
d2$delay_unit <- ifelse(str_detect(d2$delay, 'd'), 'days', 
                        ifelse(str_detect(d2$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d2$delay, 'm'), 'months',
                                      ifelse(str_detect(d2$delay, 'y'), 'years', 0))))

# pull out the number in delay variable
d2$n_unit <- ifelse(str_detect(d2$delay, '12'), '12', 
                    ifelse(str_detect(d2$delay, '4'), '4', 
                           ifelse(str_detect(d2$delay, '7'), '7',
                                  ifelse(str_detect(d2$delay, '1'), '1', 0))))


# convert k vals to numeric
#d2$kval <- paste0('.', d2$kval) # more concise
#d2$kval <- as.numeric(as.character(d2$kval))

# recode choice into LL (0) or SS (1)
d2$choice <- ifelse(d2$choice == 2, 0, 1)
d2$choice <- as.numeric(d2$choice)

# summarize data 
d3 <- summarySE(d2, measurevar = "choice", groupvars = c("frame", "delay_unit", "n_unit"))
d3$label <- paste0(d3$n_unit, ' ', d3$delay_unit)

d3$comparison <- c(1,3,2,2,1,3)

# first comparison
oneweek_vs_7days <- ggplot(d3[which(d3$comparison == 1),], aes(label, choice)) + geom_bar(stat='identity', fill = "#009E73") +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2) +
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

oneweek_vs_7days

#second comparison
onemonth_vs_4weeks <- ggplot(d3[which(d3$comparison == 2),], aes(label, choice)) + geom_bar(stat='identity', fill = "#CC79A7") +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2) +
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

onemonth_vs_4weeks

#third comparison
oneyear_vs_12months <- ggplot(d3[which(d3$comparison == 3),], aes(label, choice)) + geom_bar(stat='identity', fill = "#D55E00") +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2) + 
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

oneyear_vs_12months

## add kvalue
# summarize data 
d4 <- summarySE(d2, measurevar = "choice", groupvars = c("frame", "delay_unit", "n_unit", "kval"))
d4$label <- paste0(d4$n_unit, ' ', d4$delay_unit)

d4$comparison <- c(1,1,1,
                   3,3,3,
                   2,2,2,
                   2,2,2,
                   1,1,1,
                   3,3,3)

# first comparison
oneweek_vs_7days_vs_kval <- ggplot(d4[which(d4$comparison == 1),], aes(label, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) +
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

oneweek_vs_7days_vs_kval

#second comparison
onemonth_vs_4weeks_vs_kval <- ggplot(d4[which(d4$comparison == 2),], aes(label, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) +
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

onemonth_vs_4weeks_vs_kval

#third comparison
oneyear_vs_12months_vs_kval <- ggplot(d4[which(d4$comparison == 3),], aes(label, choice, fill = kval)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=choice-se, ymax=choice+se), width=.2, position=position_dodge(.9)) +
  theme_minimal() + ylab('Proportion SS Choice') + xlab('Framing')

oneyear_vs_12months_vs_kval

