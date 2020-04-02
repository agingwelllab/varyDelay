# Clean varydelay data
# 12.9.19 KLS and SL

# load required packages
library(here)
library(stringr)

# load source functions

# set hard-coded variables

# load data
#dt <- read.csv(here::here('data', 'pilotdata.csv'))
dt <- read.csv(here::here('data', 'varyDelay_March_6_softlaunch_n=29.csv'))
               
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
dt <- dt[-c(1:2),] # remove qualtrics header
dt$Age <- as.integer(as.character(dt$Age)) # fix age variable
#dt$Age <- dt$Age + 19

# pad sub numbers with zeros
dt$ID <- str_pad(dt$ID, 3, pad = "0")

# change variable types
dt[16:length(dt)] <- sapply(dt[16:length(dt)], as.character)
dt[16:length(dt)] <- sapply(dt[16:length(dt)], as.integer)

# add data type to data dictionary
dd$type <- sapply(dt, class)

# Create and populate allowed_values in data dictionary

# Create allowed values dd$allowed_values
dd[,'allowed_values'] <- NA

# Populate with allowed values
dd$allowed_values[grep("ID", colnames(dt))] <- '001-276'
dd$allowed_values[grep("Date", colnames(dt))] <- 'YYYY-MM-DD HH:MM:SS'
dd$allowed_values[grep("Q3", colnames(dt))] <- '1'
dd$allowed_values[grep("Q4", colnames(dt))] <- '1 = Yes, 2 = No'
dd$allowed_values[grep("Sex", colnames(dt))] <- '1 = Male, 2 = Female'
dd$allowed_values[grep("Age", colnames(dt))] <- '21-85'
dd$allowed_values[grep("Education", colnames(dt))] <- 
  "1 = Middle School, 2 = High School Diploma, 3 = Some College, 4 = Associate's Degree, 
  5 = Bachelor's Degree, 6 = Master's Degree, 7 = Professional/Doctoral Degree"
dd$allowed_values[grep("Race", colnames(dt))] <- '1 = White/Caucasian, 2 = Black/African American, 
3 = Asian, 4 = American Indian/Alaskan Native, 5 = Native Hawaiian/Pacific Islander, 6 = Multiracial, 
7 = Other'
dd$allowed_values[grep("Income", colnames(dt))] <- '
1 = less than $10,000, 
2 = $10,000-$19,999, 
3 = $20,000-$29,999, 
4 = $30,000-$39,999
5 = $40,000-$49,999, 
6 = $50,000-$59,999, 
7 = $60,000-$69,999, 
8 = $70,000-$79,999, 
9 = $80,000-$89,999, 
10 = $90,000-$99,999, 
11 = $100,000-$109,999, 
12 = $110,000-$119,999, 
13 = $120,000-$129,999, 
14 = $130,000-$139,999, 
15 = $140,000-$149,999,
16 = $150,000 or more'
dd$allowed_values[grep("physical_health", colnames(dt))] <- '1 = not healthy at all, 
2 = slightly healthy, 3 = moderately healthy, 4 = quite healthy, 5 = very healthy'
dd$allowed_values[grep("DOB_month", colnames(dt))] <- '1 = January, 2 = February, 3 = March, 4 = April, 
5 = May, 6 = June, 7 = July, 8 = August, 9 = September, 10 = October, 11 = November, 12 = December'
dd$allowed_values[grep("DOB_day", colnames(dt))] <- '1-31'
dd$allowed_values[grep("DOB_year", colnames(dt))] <- '1-100, 1 = 1920, 100 = 2020'
dd$allowed_values[grep("FTP", colnames(dt))] <- "1 - 7; 1= Very Untrue, 7 = Very True"
dd$allowed_values[grep("X", colnames(dt))] <- "1 = smaller sooner, 2 = larger later"
dd$allowed_values[grep("PR", colnames(dt))] <- "1 = smaller sooner, 2 = larger later"
dd$allowed_values[grep("CT", colnames(dt))] <- "1 = smaller sooner, 2 = larger later"
dd$allowed_values[grep("liquid_savings", colnames(dt))] <- "1=$0, 2=$1-49, 3=$50-99, 4=$100-249, 
5=$350-499, 6=$500-999, 7=$1,000-1,999, 8=$2,000-4,999, 9=$5,000-9,999, 10=$10,000-19,999, 
11=$20,000-49,000, 12=$50,000-74,999, 13=$75,000 or more"
dd$allowed_values[grep("confidence", colnames(dt))] <- 
  "1 = I am certain I could come up with the full $2,000, 
2 = I could probably come up with $2,000,
3 = I could probably not come up with $2,000, 
4 = I am certain I could not come up with $2,000, 
5 = I don't know"
dd$allowed_values[grep("investments", colnames(dt))] <- '1 = Yes, 2 = No'
dd$allowed_values[grep("health_ins", colnames(dt))] <- '1 = Yes, 2 = No'
dd$allowed_values[grep("extra_money", colnames(dt))] <- '
1 = My friends or family would lend me the money and expect me to repay them, 
2 = My friends or family would give me the money with no expectation of repayment'

# Save
write.csv(dd, here::here("data", "varydelay_data_dictionary.csv"), row.names = FALSE)
write.csv(dt, here::here("data", "varydelay_data.csv"), row.names = FALSE)




