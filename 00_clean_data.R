# Clean varydelay data
# 12.9.19 KLS and SL

# load required packages
library(here)

# load source functions

# set hard-coded variables

# load data
dt <- read.csv(here('data', 'pilotdata.csv'))
               
# add id column
dt$ID <- c(0,0,seq(1,nrow(dt)-2,1))

# clean
dt <- dt[-c(3:10)]
dt <- dt[c(ncol(dt),1:(ncol(dt)-1))]

# create data dictionary
dd <- t(dt[1,])
dd <- cbind(rownames(dd), data.frame(dd, row.names = NULL))
colnames(dd) <- c('Variable', 'Question')

# finish cleaning
dt <- dt[-c(1:2),]
dt$Age <- as.integer(as.character(dt$Age))
dt$Age <- dt$Age + 19

write.csv(dd, here("data", "varydelay_data_dictionary.csv"), row.names = FALSE)
write.csv(dt, here("data", "varydelay_data.csv"), row.names = FALSE)
