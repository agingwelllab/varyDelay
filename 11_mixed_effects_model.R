# Mixed effects model

# load required packages

library(here)
library(tidyverse)
library(plyr)
library(stats)
library(lme4)
library(sjPlot)
# load source functions

source(here::here('scr', 'transform_delay_and_k.R'))
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'summarySE.R'))

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate gamble data ####
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create variables for mixed effect model

d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

d1 <- create_delay_n_days(d1)
d1 <- create_k_value(d1)
d1 <- create_delay_unit(d1)

# Scale variables for analysis
d1$Age <- scale(d1$Age, center = TRUE, scale=TRUE)
d1$logdnd <- log(d1$delay_n_days)

# recode choice into LL (1) or SS (0)
d1$choice <- ifelse(d1$choice == 2, 0, 1)
d1$choice <- as.numeric(d1$choice)

# Mixed Effect Model

M4 <- glmer(data = d1, choice ~ Age * kval * delay_n_days + (1|delay_unit:delay_n_days), family = binomial)
summary(M4)

# Table of model results
 
#Std. Beta
tab_model(M4,
    show.est = FALSE,      
    show.std = TRUE,
    dv.labels = c("Choice"),
    digits = 3,
    string.std = "Std. Beta",
    string.std_ci = "Std. 95% CI",
    p.style = "numeric_stars"
    )

#Estimate (Called Log-Odds in table, can be changed with string.est = "preferred name")
tab_model(M4,
          transform = NULL,
          show.est = TRUE, 
          show.stat = TRUE,
          dv.labels = c("Choice"),
          digits = 3
)
