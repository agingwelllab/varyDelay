# 1.29.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(ez)
library(sjPlot)
library(knitr)

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

# 4 (delay_unit) x Age Within-Subjects ANCOVA
m1 <- ezANOVA(data = d0, dv = .(choice), wid = .(ID), within = .(delay_unit), between = .(Age), return_aov = TRUE, detailed = TRUE)
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

#overall means and comparison
d6 <- colMeans(d1[3:6])
all <- pairedttable(d1[c(1,3:6)], colnames(d1[3:6]))

# between-subs t-tests
days_t <- t.test(d2$days, d3$days)
weeks_t <- t.test(d2$weeks, d3$weeks)
months_t <- t.test(d2$months, d3$months)
years_t <- t.test(d2$years, d3$years)

# Reorder data for tables
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)

# sjPlot table for Fig. 1 (Saved as CSV)

tab_df(d1[, c(1, 3:4)],
        col.header = c("Delay Unit", "Choice", "Std. Deviation"),
        title = "Mean and standard deviation of proportion of smaller, sooner options selected by delay length")       

# Post-hoc test for M1
PHM1 <- pairwise.t.test(d0$choice, d0$delay_unit, paired = T, p.adjust.method = "holm")
M1PHT <-as.table(PHM1$p.value)

knitr::kable(list(M1PHT))
write.csv(M1PHT, here::here("Post_Hoc_Test_M1.csv"))


# Mixed effect model

MM1 <- lmer(choice~Age + (1| delay_unit) +(1| kval), data = d0)
summary(MM1)


