# Examine questionnaire results
# 2.19.20 KLS and SL

# load required packages
library(here)
library(plyr)
library(ggplot2)

# load source functions
source(here::here('scr', 'isolate_data.R'))
source(here::here('scr', 'calculate_ftp.R'))

# set hard-coded variables

# load data
dt <- read.csv(here::here('data', 'varydelay_data.csv'))

# ==================== 
# Future Time Perspectives
# ==================== 
ftp <- calculate_ftp(dt)

## calculate number of bins needed
bins = seq(min(ftp$FTP, na.rm=TRUE), round(max(ftp$FTP, na.rm=TRUE)), 0.5)

# #build histogram
hist(ftp$FTP, breaks= bins, col= "grey")

# ==================== 
# liquid savings
# ==================== 
dt$liquid_savings <- mapvalues(dt$liquid_savings, 
            from = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16'), 
            to = c('less than $10,000', '$10,000-$19,999', '$20,000-$29,999', '$30,000-$39,999', 
                   '$40,000-$49,999', '$50,000-$59,999', '$60,000-$69,999', '$70,000-$79,999', 
                   '$80,000-$89,999', '$90,000-$99,999', '$100,000-$109,999', '$110,000-$119,999', '
                   $120,000-$129,999', '$130,000-$139,999', '$140,000-$149,999','$150,000 or more'))
savings <- count(dt$liquid_savings)
names(savings) [1] = "Savings"
savings <- savings[which(savings$Savings != 'NA'),]

#make into piechart
title <- "How much money do you have in savings today (in cash, checking, and savings account balances)?"
ggplot(savings, aes(x="", y= freq, fill= Savings)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + ggtitle(title)
rm(savings)

# ==================== 
# confidence
# ==================== 
#dt$confidence <- mapvalues(dt$confidence, from = c('1', '2'), to = c('Yes', 'No'))
dt$confidence <- mapvalues(dt$confidence, from = c('1', '2', '3', '4','5'), 
                           to = c("I am certain I could come up with the full $2,000", 
                                  "I could probably come up with $2,000", 
                                  "I could probably not come up with $2,000", 
                                  "I am certain I could not come up with $2,000", 
                                  "I don't know"))

conf <- count(dt$confidence)
names(conf) [1] = "Confidence"
conf <- conf[which(conf$Confidence != 'NA'),]

#make into piechart
title <- "How confident are you to unexpectedly come up with $2,000 in 30 days?"
ggplot(conf, aes(x="", y= freq, fill= Confidence)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + ggtitle(title)
rm(conf)

# ==================== 
# investments
# ==================== 
dt$investments <- mapvalues(dt$investments, from = c('1', '2'), to = c('Yes', 'No'))
invest <- count(dt$investments)
names(invest) [1] = "invest"
invest <- invest[which(invest$invest != 'NA'),]

#make into piechart
title <- "Do you currently have non-retirement investments?"
ggplot(invest, aes(x="", y= freq, fill= invest)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + ggtitle(title)
rm(invest)

# ==================== 
# health insurance
# ==================== 
dt$health_ins <- mapvalues(dt$health_ins, from = c('1', '2'), to = c('Yes', 'No'))
hins <- count(dt$health_ins)
names(hins) [1] = "Health"
hins <- hins[which(hins$Health != 'NA'),]

#make into piechart
title <- "Do you currenly have health insurance?"
ggplot(hins, aes(x="", y= freq, fill= Health)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + ggtitle(title)
rm(hins)

# ==================== 
# extra money
# ==================== 
#data key (qualtrics): 1= yes- have to repay; 2- yes- not have to repay; NA=NA
dt$extra_money <- mapvalues(dt$extra_money, from = c('1', '2', 'NA'), 
to = c("Yes and expect me to repay", 
       "Yes with no expectation of repayment", "NA"))
money <- count(dt$extra_money)
names(money) [1] = "extra_money"
money <- money[which(money$extra_money != 'NA'),]

#make into piechart
title <- "Would friends/family loan money and expect to be repaid?"
ggplot(money, aes(x="", y= freq, fill= extra_money)) + geom_bar (stat= "identity", width=1) +
  coord_polar ("y", start = 0) + theme(axis.text.x=element_blank()) + ggtitle(title)
rm(money)
