# 2.26.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(stats)
library(reghelper)
library(stargazer)
library(broom)

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

# transform delay_n_days variable
hist(d1$delay_n_days)

d1$sqrtdd <- sqrt(d1$delay_n_days)
hist(d1$sqrtdd)

d1$logdnd <- log(d1$delay_n_days)
hist(d1$logdnd)
#logdnd wins!

# Simple Logistic Regression
#M2 <- glm(d1$choice ~ d1$Age * d1$delay_n_days, family = binomial(link = 'logit'), data = d1)
#summary(M2)

M2 <- glm(d1$choice ~ d1$Age * d1$logdnd, family = binomial(link = 'logit'), data = d1)
summary(M2) 

# Generate post-hoc test variables.

probabilities <- predict(M2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Positive", "Negative")

# Test for linearity
# Collect variables and graph for linear logit relationship
PHT <- d1 %>%
  dplyr::select(c('choice', 'Age', 'logdnd')) 
predictors <- colnames(d1)
# Bind the logit and tidying the data for plot
PHT <- PHT %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(PHT, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

d1$LAge <- log(d1$Age)*d1$Age
#d1$LDelay <- log(d1$delay_n_days)*d1$delay_n_days
d1$LDelay <- log(d1$logdnd)*d1$logdnd
#L.Test <- glm(choice~Age + delay_n_days + LDelay + LAge, data = d1, family = binomial())
L.Test <- glm(choice~Age + logdnd + LDelay + LAge, data = d1, family = binomial())
summary(L.Test)

# Influential values model

plot(M2, which = 4, id.n = 3)
model.data <- augment(M2) %>% 
  dplyr::mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = d1$delay_n_days), alpha = .5) +
  theme_bw()


# Discount rate added regression model
#M3 <- glm(d1$choice ~ d1$Age * d1$delay_n_days * d1$kval, family = binomial(link = 'logit'), data = d1)
#summary(M3)

M3 <- glm(d1$choice ~ d1$Age * d1$logdnd * d1$kval, family = binomial(link = 'logit'), data = d1)
summary(M3)


#find standardzied coefficients (beta)- now this works!
standardized_betas <- beta(M2)

# Stargazer Tables
# Kval table
stargazer(M2, M3, 
          type="html", 
          dep.var.labels = c("Choice"), 
          star.cutoffs = c(.05, .01, .001),
          covariate.labels = c("Age", "Delay in Days", "Discount Rate", "Age x Delay in Days", "Age x Discount Rate", "Delay in Days x Discount Rate", "Age x Delay in Days x Discount Rate"
          ), ci=TRUE, ci.level = 0.95, summary = FALSE)
