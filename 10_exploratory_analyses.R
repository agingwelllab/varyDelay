# 6.23.20 KLS, SL
# choice 1 = SS, choice 2 = LL until we change it!

# load required packages
library(here)
library(plyr)
library(tidyverse)
library(Hmisc)

# load source functions
source(here::here('scr', 'calculate_ftp.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# score FTP
ftp <- calculate_ftp(dt)
ftp$ftp <- ftp$FTP; ftp$FTP <- NULL
#ftp <- ftp %>% rename(ftp = FTP)
dt <- merge(dt, ftp)
rm(ftp)

# create matrix for correlation
columns <- c(grep('Age', colnames(dt)), grep('ftp', colnames(dt)), grep('liquid_savings', colnames(dt)), grep('confidence', colnames(dt)),
             grep('investments', colnames(dt)), grep('extra_money', colnames(dt)), grep('health_ins', colnames(dt)))
cor_matrix <- as.matrix(dt[columns])

# correlation between age and FTP and safety net 
cor_table <- rcorr(cor_matrix)
rm(cor_matrix, columns)

# Does ftp mediate the relationship between and and choice?
# isolate gamble data
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days

d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

# break apart gamble type into delay and k val columns
d1$delay <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,1]) #create delay column
d1$kval <- as.factor(t(as.data.frame(strsplit(d1$gambletype, '_')))[,2]) # create k val column

# convert the letter in delay variable to # of days (d for day = 1, w for week = 7, m for month = 30, 
# y for year = 365)
d1$delay_n_days <- ifelse(str_detect(d1$delay, 'd'), '1', 
                          ifelse(str_detect(d1$delay, 'w'), '7', 
                                 ifelse(str_detect(d1$delay, 'm'), '30',
                                        ifelse(str_detect(d1$delay, 'y'), '365', 0))))

# isolate the number from delay variable 
d1$delay <- str_remove_all(d1$delay, "[Xdwmy]")
#d1$gambletype <- NULL # remove gamble type variable

d1$delay <- as.numeric(d1$delay)
d1$delay_n_days <- as.numeric(d1$delay_n_days)
d1$delay_n_days <- d1$delay*d1$delay_n_days
d1$delay_n_days <- as.numeric(str_replace(d1$delay_n_days, "28", "30")) 
d1$delay_n_days <- as.numeric(str_replace(d1$delay_n_days, "360", "365"))

# convert k vals to numeric
d1$kval <- paste0('.', d1$kval) # more concise
d1$kval <- as.numeric(as.character(d1$kval))

# recode choice into LL (1) or SS (0)
d1$choice <- ifelse(d1$choice == 2, 0, 1)
d1$choice <- as.numeric(d1$choice)

# add FTP to d1
d2 <- merge(d1, dt[c(1,96)])

# Step 1 - age on FTP
step1 <- lm(ftp ~ Age, data = d2[which(d2$delay_n_days == 3650),])
summary(step1)

# Step 2 - FTP on choice
step2 <- glm(choice ~ ftp, data = d2[which(d2$delay_n_days == 3650),])
summary(step2)

# Step 3 - age + FTP on choice
step3 <- glm(choice ~ ftp + Age, data = d2[which(d2$delay_n_days == 3650),])
summary(step3)

#Step 0- age on choice
step0 <- glm(choice ~ Age, data = d2[which(d2$delay_n_days == 3650),])
summary(step0)

#find standardized betas
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}
stdz.coff(step1)
stdz.coff(step2)
stdz.coff(step3)
stdz.coff(step0)
