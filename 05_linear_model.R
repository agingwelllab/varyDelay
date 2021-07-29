# 2.26.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(stats)
library(reghelper)
library(broom)
library(sjPlot)
library(rmcorr)

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))
source(here::here('scr', 'logistic_pseudoR2.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data ####
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

# Create factor version of logdnd (For use in correlation matrix)
d1$logdndl <- factor(d1$logdnd,
                     levels = c(0, 1.38629436111989, 1.94591014905531, 2.63905732961526, 3.40119738166216, 5.19295685089021, 5.89989735358249, 7.50933526601659, 8.20248244657654),
                     labels = c("1", "4", "7", "14", "30", "180", "365", "1825", "3650"))


# Simple Logistic Regression ####
#M2 <- glm(d1$choice ~ d1$Age * d1$delay_n_days, family = binomial(link = 'logit'), data = d1)
#summary(M2)

M2 <- glm(choice ~ Age * logdnd, family = binomial(link = 'logit'), data = d1)
SM2 <- summary.glm(M2, correlation = TRUE, signif.stars = TRUE) 
print(SM2)

#find standardzied coefficients (beta)
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}
stdz.coff(M2)

# Means(SDs) and follow-up tests
d2 <- d1 %>% dplyr::group_by(delay_n_days) %>% dplyr::summarize(
  mean = round(mean(choice), 2),  
  sd = round(sd(choice), 2), 
  estimate = cor.test(Age, choice)$estimate, 
  pvalue = cor.test(Age, choice)$p.value 
)


d2 <- d2 %>% mutate(
  estimate = round(estimate,3),
  sig05 = pvalue < .05 & (!is.na(pvalue)),
  sig01 = pvalue < .01  & (!is.na(pvalue)),
  sig001 = pvalue < .001  & (!is.na(pvalue)),
  stars = ifelse(sig001 == TRUE, "***", 
         ifelse(sig01 == TRUE, "**", 
         ifelse(sig05 == TRUE, "*", ""))), 
  combined_est = paste0(as.character(estimate), stars)
)

d2 <- d2[c(1:3, 10)]

write.csv(d2, here::here('figs', 'means_sd_corr_M2.csv'), row.names = FALSE)

# Assumptions Testing ####

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

# Additional regression models ####
# Discount rate added regression model
#M3 <- glm(d1$choice ~ d1$Age * d1$delay_n_days * d1$kval, family = binomial(link = 'logit'), data = d1)
#summary(M3)

M3 <- glm(choice ~ Age * logdnd * kval, family = binomial(link = 'logit'), data = d1)
summary(M3)




# #Build Correlation Matrix Table - I don't think this code does what you think it does....
# 
# M2C <- glm(choice ~ Age * logdndl, family = binomial(link = 'logit'), data = d1)
# SM2C <- summary.glm(M2C, 
#                     correlation = TRUE) 
# print(SM2C$correlation[c(2, 11:18), c(2, 11:18)])
# 
# # Table of correlations between proportion of smaller, sooner options chosne and age, by length of delay in log days (Table 3)
# 
# tab_corr(SM2C$correlation[c(2, 11:18), c(2, 11:18)], 
#          triangle = "l",
#          show.p = TRUE, p.numeric = TRUE,
#          title = "Correlation of age and proportion of smaller sooner options, by log of delay in days",
#          var.labels = c("Log 1", " Log 4", "Log 7", "Log 14", "Log 30", "Log 180", "Log 365", "Log 1825", "Log 3650")
#          )

# sjPlot Tables (Table 4)

tab_model(M2, M3, 
          show.std = TRUE,
          show.est = FALSE,
          show.p = TRUE,
          collapse.ci = TRUE,
          dv.labels = c("Original Model 2", "Discount Model"),
          pred.labels = c("Intercept", "Age", "Log Delay in Day", "Age * Log Delay in Days", "Discount Rate", "Age * Discount Rate", "Log Delay in Days * Discount Rate", "Age * Log Delay in Days * Discount Rate")
)

