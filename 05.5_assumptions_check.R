# 7.30.21 CRG, KLS
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here::here('scr', 'transform_delay_and_k.R'))
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# load model 2
M2 <- readRDS(here::here('output', 'model2.RDS'))

# isolate gamble data ####
gd <- isolate_data(dt, grep('ID', colnames(dt))[1], c(grep('Age', colnames(dt)), 
                                                      grep('X1d_1', colnames(dt)):grep('X10y_005', colnames(dt))))
gd <- gd[complete.cases(gd),]

# create new variable/model - delay_n_days
d1 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005) # make gambletype column

d1 <- create_delay_n_days(d1)
d1 <- create_k_value(d1)

# transform delay_n_days variable
d1$logdnd <- log(d1$delay_n_days)

# Check assumptions ####

# Test for linearity
# Collect variables and graph for linear logit relationship
PHT <- d1 %>%
  dplyr::select(c('choice', 'Age', 'logdnd')) 
predictors <- colnames(d1)

# Bind the logit and tidying the data for plot - something is going wrong here
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
