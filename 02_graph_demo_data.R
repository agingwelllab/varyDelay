# Examine demographic data
# 12.9.19 KLS and SL

# load required packages
library(here)
library(plyr)
library(ggplot2)


# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# isolate demo data
demo <- isolate_data(dt, grep('ID', colnames(dt)), 
                     grep('Sex', colnames(dt)):grep('physical_health', colnames(dt))) 
# ====================                     
# pie of education
# ====================
# TBD

# ====================
# pie chart of race
# ====================
demo$Race <- factor(demo$Race)

demo$Race <- mapvalues(demo$Race, from = c('1', '2', '3', '5', '6', '7', '8'), 
                       to = c('White/Caucasian', 
                              'Black/African American', 
                              'Asian', 
                              'American Indian/ Alaska Native', 
                              'Native Hawaiian/Island Pacificer', 
                              'Multiracial', 
                              'Other'))
#pie(table(demo$Race))
#get Race Frequencies & save as its own table
Race_frequency <- count(demo$Race)
Race_frequency <- Race_frequency [c(1:3),]
Race_frequency <- count(demo$Race)

#rename to race & frequency
names(Race_frequency) [1] = "Race"

# add percentage
Race_frequency$perc <- round(Race_frequency$freq/nrow(demo)*100,2)

#make it circular with coord_polar()
race <- ggplot(Race_frequency, aes(x="", y= freq, fill= Race)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + theme_void()
rm(Race_frequency)

# ====================
# histogram of income
# ====================
demo$Income <- ordered(demo$Income, levels = as.character(seq(1,16,1)))
demo$Income <- mapvalues(demo$Income, from = as.character(seq(1,16,1)), 
                         to = c('< $10,000', '$10,000-$19,999', '$20,000-$29,999', 
                                '30,000-$39,999', '$40,000-$49,999','$50,000-$59,999', 
                                '$60,000-$69,999', '$70,000-$79,999', '$80,000-$89,999', 
                                '$90,000-$99,999', '$100,000-$109,999', '$110,000-$119,999',
                                '$120,000-$129,999', '$130,000-$139,999', '$140,000-$149,999',
                                '>= $150,000') )

income <- ggplot(demo, aes(Income)) + geom_histogram(stat='count') + 
  theme_minimal() + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))
income
# ====================
# histogram of age
# ====================
## calculate number of bins needed
#bins = seq(min(dt$Age, na.rm=TRUE), max(dt$Age, na.rm=TRUE), 10)

# #build histogram
#age <- hist(dt$Age, breaks= length(bins), col= "grey")
#age <- ggplot(demo, aes(Age)) + geom_histogram(stat='count') + theme_minimal() 
#age
hist(dt$Age)
