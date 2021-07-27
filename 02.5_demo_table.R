# Table of demographic data
# 7.27.21 KLS 

# load required packages
library(here)
library(tidyverse)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate demo data
demo <- isolate_data(dt, grep('ID', colnames(dt)), 
                     grep('Sex', colnames(dt)):grep('physical_health', colnames(dt))) 

# create summary table by median split on age
demo$agegroup <- ifelse(demo$Age > median(demo$Age), 'Older', 'Younger')

tb <- demo %>% group_by(agegroup) %>% summarize(
  n = n(),
  mAge = mean(Age), 
  sdAge = sd(Age),
  mEducation = mean(Education), 
  sdEducation = sd(Education),
  mIncome = mean(Income),
  sdIncome = sd(Income),
  mhealth = mean(physical_health),
  sdhealth = sd(physical_health)
)

demo$SexLabel <- recode(demo$Sex, "1" = "M", "2" = "F")

demo$EducationLabel <- recode(demo$Education, "1" = "Middle School", "2" = "High School Diploma", 
                              "3" = "Some College", "4" = "Associate's Degree", "5" = "Bachelor's Degree", 
                              "6" = "Master's Degree", "7" = "Professional/Doctoral Degree")

demo$RaceLabel <- recode(demo$Race, "1" = "White/Caucasian", "2" = "Black/African American", 
                         "3" = "Asian", "5" = "American Indian/ Alaska Native", 
                         "6" = "Native Hawaiian/Island Pacificer", "7" = "Multiracial", 
                         "8" = "Other") 

demo$IncomeLabel = recode(demo$Income, '1' = '< $10,000', '2' = '$10,000-$19,999', '3' = '$20,000-$29,999',
                          '4' = '30,000-$39,999', '5' = '$40,000-$49,999','6'= '$50,000-$59,999', 
                          '7' = '$60,000-$69,999', '8' ='$70,000-$79,999', '9' = '$80,000-$89,999', 
                          '10' = '$90,000-$99,999', '11' = '$100,000-$109,999', '12' = '$110,000-$119,999',
                          '13' = '$120,000-$129,999', '14' = '$130,000-$139,999', '15' = '$140,000-$149,999',
                          '16' = '>= $150,000') 

sex <- demo %>% group_by(agegroup) %>% count(SexLabel)
race <- demo %>% group_by(agegroup) %>% count(RaceLabel)

# make pretty table
tb <- t(tb)
colnames(tb) <- tb[1,]
tb <- tb[2:10,]
t2 <- tb
t2 <- t2[1:7,]
# age mean (sd)
t2[2,1] <- paste0(round(as.numeric(tb[2,1]),2),  ' (', round(as.numeric(tb[3,1]),2), ') ')
t2[2,2] <- paste0(round(as.numeric(tb[2,2]),2),  ' (', round(as.numeric(tb[3,2]),2), ') ')
# education  mean (sd)
t2[3,1] <- paste0(round(as.numeric(tb[4,1]),2),  ' (', round(as.numeric(tb[5,1]),2), ') ')
t2[3,2] <- paste0(round(as.numeric(tb[4,2]),2),  ' (', round(as.numeric(tb[5,2]),2), ') ')
# income mean (sd)
t2[4,1] <- paste0(round(as.numeric(tb[6,1]),2),  ' (', round(as.numeric(tb[7,1]),2), ') ')
t2[4,2] <- paste0(round(as.numeric(tb[6,2]),2),  ' (', round(as.numeric(tb[7,2]),2), ') ')
# health mean (sd)
t2[5,1] <- paste0(round(as.numeric(tb[8,1]),2),  ' (', round(as.numeric(tb[9,1]),2), ') ')
t2[5,2] <- paste0(round(as.numeric(tb[8,2]),2),  ' (', round(as.numeric(tb[9,2]),2), ') ')

# add sex 
t2[6,1] <- paste0(sex$SexLabel[1], ' = ', sex$n[1], ', ', sex$SexLabel[2], ' = ', sex$n[2])
t2[6,2] <- paste0(sex$SexLabel[3], ' = ', sex$n[3], ', ', sex$SexLabel[4], ' = ', sex$n[4])

# add race
t2[7,1] <- paste0(race$RaceLabel[1], ' = ', race$n[1], ', ', race$RaceLabel[2], ' = ', race$n[2], ', ',
                  race$RaceLabel[3], ' = ', race$n[3], ', ', race$RaceLabel[4], ' = ', race$n[5],', ',
                  race$RaceLabel[5], ' = ', race$n[5], ', ', race$RaceLabel[6], ' = ', race$n[6])
t2[7,2] <- paste0(race$RaceLabel[7], ' = ', race$n[7], ', ', race$RaceLabel[8], ' = ', race$n[8], ', ',
                  race$RaceLabel[9], ' = ', race$n[9], ', ', race$RaceLabel[10], ' = ', race$n[10],', ',
                  race$RaceLabel[11], ' = ', race$n[11], ', ', race$RaceLabel[12], ' = ', race$n[12])


rownames(t2) <- c("N", "Age", "Education", "Income", "Physical Health", "Race", "Sex")

# save
write.csv(t2, here::here('figs', 'demo_table.csv'))
