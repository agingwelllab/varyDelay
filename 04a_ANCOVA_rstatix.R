# 10.14.20 SL
# choice 1 = SS, choice 2 = LL

# load required packages- in order for rstatix to work, make sure to update dplyr package 
library(here)
library(plyr)
library(tidyverse)
library(rstatix)
library(ez)

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))
source(here::here('scr', 'pairedtable.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))

gd <- gd[complete.cases(gd),]

# restructuring to wide for rstatix- measurement for days, weeks, months, years for each part
gd_rstatix <- dplyr::mutate(gd, mean_days= ((gd$X1d_1 + gd$X1d_05 + gd$X1d_005 + 
                                            gd$X4d_005 + gd$X4d_05 + gd$X4d_1 + 
                                            gd$X7d_005 + gd$X7d_05 + gd$X7d_1)/9))
gd_rstatix <- dplyr::mutate(gd_rstatix, mean_weeks= ((gd$X1w_005 + gd$X1w_05 + gd$X1w_1 +
                                                gd$X2w_005 + gd$X2w_05 + gd$X2w_1 +
                                                gd$X4w_005 + gd$X4w_05 + gd$X4w_1)/9))
gd_rstatix <- dplyr::mutate(gd_rstatix, mean_months= ((gd$X1m_005 + gd$X1m_05 + gd$X1m_1 +
                                                gd$X6m_005 + gd$X6m_05 + gd$X6m_1 +
                                                gd$X12m_005 + gd$X12m_05 + gd$X12m_1)/9))
gd_rstatix <- dplyr::mutate(gd_rstatix, mean_years= ((gd$X1y_005 + gd$X1y_05 + gd$X1y_1 +
                                                 gd$X5y_005 + gd$X5y_05 + gd$X5y_1 +
                                                 gd$X10y_005 + gd$X10y_05 + gd$X10y_1)/9))
# turns out that for repeated measures, needs to be in long format!

# restructure data for analysis 
d0 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005)
#rm(gd)

d0$delay <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,1])
d0$kval <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,2])

d0$delay_unit <- ifelse(str_detect(d0$delay, 'd'), 'days', 
                        ifelse(str_detect(d0$delay, 'w'), 'weeks', 
                               ifelse(str_detect(d0$delay, 'm'), 'months',
                                      ifelse(str_detect(d0$delay, 'y'), 'years', 0))))

## Center age
d0$Age <- scale(d0$Age, center = TRUE, scale=TRUE)
gd_rstatix$Age <- scale(gd_rstatix$Age, center = TRUE, scale=TRUE)
# choice 1 = SS, choice 2 = LL

# rstatix- 4 (dely_unit, within) x age (between) ANCOVA (mixed)
d0$Age <- as.vector(d0$Age)
d0$delay_unit <- as.factor(d0$delay_unit)
d0$ID <- as.factor(d0$ID)
d0$choice <- as.numeric(d0$choice)

m1_rstatix <- anova_test (data=d0, choice ~ Age*delay_unit, wid=ID, within= delay_unit, covariate = Age)
#that runs, but I don't know if it's doing an repeated test- v. different output than m1 & df's are v. high!
#testing adding covariate to equation
m2_rstatix <- anova_test(data=d0, choice ~ Age + Age*delay_unit, wid=ID, within= delay_unit) #don't think Age is seen as within variable
#m1_rstatix=m2_rstatix
m3_rstatix <- anova_test(data = d0, dv = choice, wid = ID, within = delay_unit, coviariate= Age, effect.size = "pes") #this should be the correct one, saying error b/c of NA's- which we dont have- recommended ezANOVA 
m4_rstatix <- aov(choice ~ Age*delay_unit + Error(ID/delay_unit), data=d0) #this gives correct df- definitely within, but no idea where mauchley's is

#next: try to get GG corrections to work w/ ezANOVA!
# 4 (delay_unit) x Age Within-Subjects ANCOVA
m1 <- ezANOVA(data = d0, dv = .(choice), wid = .(ID), within = .(delay_unit), between = .(Age))
saveRDS(m1, here::here('output', 'model1.RDS'))

d0$delay <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,1])
d0$kval <- as.factor(t(as.data.frame(strsplit(d0$gambletype, '_')))[,2])

# recode choice into LL (0) or SS (1)
d0$choice <- ifelse(d0$choice == 2, 0, 1)
d0$choice <- as.numeric(d0$choice)

d0$agegrp <- ifelse(d0$Age > median(d0$Age), 'Older', 'Younger')

# get means for each delay unit
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("ID", "agegrp","delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)

d1 <- d1 %>% pivot_wider(id_cols = c('ID', 'agegrp'),  names_from = delay_unit, values_from = choice)
d2 <- d1[which(d1$agegrp == 'Younger'), ]
d3 <- d1[which(d1$agegrp == 'Older'), ]

younger <- pairedttable(d2[c(1,3:6)], colnames(d2[3:6]))
younger$tval
younger$`p values`
saveRDS(younger, here::here('output', 'youngerttest.RDS'))

older <- pairedttable(d3[c(1,3:6)], colnames(d3[3:6]))
older$tval
older$`p values`           
saveRDS(older, here::here('output', 'olderttest.RDS'))

# between-subs t-tests
days_t <- t.test(d2$days, d3$days)
weeks_t <- t.test(d2$weeks, d3$weeks)
months_t <- t.test(d2$months, d3$months)
years_t <- t.test(d2$years, d3$years)


