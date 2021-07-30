# 1.29.20 KLS, CRG, SL
# choice 1 = SS, choice 2 = LL

# load required packages
library(here)
library(tidyverse)
library(plyr)
library(ez)
library(sjPlot)
library(knitr)
library(lme4)
library(psych)

# load source functions
source(here::here('scr', 'transform_delay_and_k.R'))
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

# restructure data for analysis ####
d0 <- gather(gd, "gambletype", "choice", X1d_1:X10y_005)
rm(gd)

d0 <- create_delay_unit(d0)

# center age
d0$Age <- scale(d0$Age, center = TRUE, scale=TRUE)

# choice 1 = SS, choice 2 = LL
# recode choice into LL (0) or SS (1)
d0$choice <- ifelse(d0$choice == 2, 0, 1)
d0$choice <- as.numeric(d0$choice)

# 4 (delay_unit) x Age Within-Subjects ANCOVA ####
m1 <- ezANOVA(data = d0, dv = .(choice), wid = .(ID), within = .(delay_unit), between = .(Age), return_aov = TRUE, detailed = TRUE)
saveRDS(m1, here::here('output', 'model1.RDS'))

# Follow-up tests ####

# main effect of delay unit ####
d1 <- summarySE(data=d0, measurevar="choice", groupvars=c("ID", 'Age', "delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)
d1 <- d1 %>% pivot_wider(id_cols = c('ID', 'Age'),  names_from = delay_unit, values_from = choice)

#overall means and comparison
d6 <- colMeans(d1[3:6])#d6 <- colMeans(d1[4:7])
all <- pairedttable(d1[c(1,3:6)], colnames(d1[3:6])) #all <- pairedttable(d1[c(1,4:7)], colnames(d1[4:7]))

# Reorder data for tables
d2 <- summarySE(data=d0, measurevar="choice", groupvars=c("delay_unit"), na.rm=FALSE, conf.interval=.95, .drop=TRUE)

# sjPlot table for Fig. 1 (Saved as CSV)

tab_df(d2[, c(1, 3:4)],
        col.header = c("Delay Unit", "Choice", "Std. Deviation"),
        title = "Mean and standard deviation of proportion of smaller, sooner options selected by delay length")       

# Post-hoc test for M1
PHM1 <- pairwise.t.test(d0$choice, d0$delay_unit, paired = T, p.adjust.method = "holm")
M1PHT <-as.table(PHM1$p.value)
M1PHT <-t(round(M1PHT, 3))
 
knitr::kable(list(M1PHT))
write.csv(M1PHT, here::here("figs", "Post_Hoc_Test_M1.csv"))
dev.off()

# interaction between age and delay_unit 
days <- cor.test(d1$Age, d1$days)
weeks <- cor.test(d1$Age, d1$weeks)
months <- cor.test(d1$Age, d1$months)
years <- cor.test(d1$Age, d1$years)

add_sig <- function(x) {
  r <- round(x$estimate, 3) 
  p <- x$p.value
  sig05 <- p < .05 & (!is.na(p)); sig01 <- p < .01  & (!is.na(p)); sig001 <- p < .001  & (!is.na(p))
  if (sig05 == TRUE) {r <- paste0(as.character(r), "*")}
  if (sig01 == TRUE) {r <- paste0(as.character(r), "*")}
  if (sig001 == TRUE) {r <- paste0(as.character(r), "*")}
  return(r)
}

cAge = c(add_sig(days), add_sig(weeks), add_sig(months), add_sig(years))

write.csv(cAge, here::here('figs', 'Post_Hoc_Test_Interaction_M1.csv'))

# Mixed effect model ####

MM1 <- lmer(choice~Age + (1| delay_unit) +(1| kval), data = d0)
summary(MM1)


