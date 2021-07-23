# 2.26.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(stats)
#install.packages("reghelper")
library("reghelper")
library("stargazer")

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))
source(here::here('scr', 'logistic_pseudoR2.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

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

# By Age
d1$agegrp <- ifelse(d1$Age > median(d1$Age), 'Older', 'Younger')

# Simple Logistic Regression
M2 <- glm(d1$choice ~ d1$Age * d1$delay_n_days, family = binomial(link = 'logit'), data = d1)
summary(M2)

# Generate post-hoc test variables.

LogisticPseudoR2s(M2)
d1$predicted.probabilites<-fitted(M2)
d1$standardized.residuals<-rstandard(M2)
d1$studentized.residuals<-rstudent(M2)
d1$dfbeta<-dfbeta(M2)
d1$dffit<-dffits(M2)
d1$leverage<-hatvalues(M2)

# Test for linearity
d1$logDelay_in_days <- log(d1$delay_n_days)*d1$delay_n_days
d1$logAge <- log(d1$Age)*d1$Age
M2LinearTest <- glm(d1$choice~d1$Age * d1$delay_n_days + d1$logDelay_in_days * d1$logAge, family = binomial(link = "logit"), data = d1 )
summary(M2LinearTest)


# Discount rate added regression model
M3 <- glm(d1$choice ~ d1$Age * d1$delay_n_days * d1$kval, family = binomial(link = 'logit'), data = d1)
summary(M3)


#find standardzied coefficients (beta)- this didnt work
standardized_betas <- beta(M2)

# standardize (mean center/sd) all variables in model
d1 <- d1 %>% mutate(age_standard = ((Age-mean(Age))/sd(Age)))
d1 <- d1 %>% mutate(delay_n_days_standard = ((delay_n_days-mean(delay_n_days))/sd(delay_n_days)))
d1 <- d1 %>% mutate(age_scale= scale(Age, center = TRUE, scale = TRUE))

#double check standardization
#hist(d1$Age)
#hist(d1$age_standard)
#hist(d1$delay_n_days)
#hist(d1$delay_n_days_standard)
#hist(d1$Age, breaks = 10)
#hist(d1$age_standard, breaks = 10)

#Re-run logistic regression model w/ standardized variables to get standardized betas
M2_standard <- glm(d1$choice ~ d1$age_standard * d1$delay_n_days_standard, family = binomial(link = 'logit'), data = d1)
summary(M2_standard)
# we don't think these are right, because the M2 and M2_standard model results are different

# Alternatate Standardized Coefficients via function
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}

stdz.coff(M2)
#hopefully these beta's are right!

# Stargazer Tables
# Kval table
stargazer(M2, M3, 
          type="html", 
          dep.var.labels = c("Choice"), 
          covariate.labels = c("Age", "Delay in Days", "Discount Rate", "Age x Delay in Days", "Age x Discount Rate", "Delay in Days x Discount Rate", "Age x Delay in Days x Discount Rate"
          ), ci=TRUE, ci.level = 0.95, summary = FALSE)