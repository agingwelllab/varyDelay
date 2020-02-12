#histogram of liquid_savings
#SL 2.11.20

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

#extract liquid_savings from data
liquid_savings <- dt$liquid_savings

######   this section is not done!!!! #####- maybe try bar graph instead!
# create savings category labels
savings_cat <- c("$0", "$1-49", "$50-99", "$100-249", "$250-499", "$500-999", "$1,000-1,999", "$2,000-4,999", "$5,000-9,999", "$10,000-19,999", "$20,000-49,999", "$50,000-74,999", "$75,000 or more")


#the liquid_savings question has 13 possible ordinal categories
#creates histogram of liquid_savings with 13 bins, counting by 1 on x-axis
hist(liquid_savings, col="gray", xlim = c(0,13), breaks=seq(1,13,1))

#**update: think we need to look into bar graph instead! http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
#- but then need table with name & count!
table(dt$liquid_savings)
#add name and count to table