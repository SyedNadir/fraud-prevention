
# Clear environment
rm(list = ls()) 

# Clear packages
pacman::p_unload(rgl)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

getwd()
setwd("U:/My Docs/Data Science/14 weeks training/Projects/Fraud Prevention/rstudio")

### Task 1: Consolidate data into one table

txns <- read.csv('ecom_txns.csv')
ip <- read.csv('ip_mappings.csv')

#check if data loaded correctly
str(txns)
str(ip)
head(txns)
tail(txns)
head(ip)
tail(ip)

# remove ip_mappings Observations with bad ip address
ip <- as.data.frame(head(ip,421239))

# first convert ip addresses to int
# ip address a.b.c.d is decimal representation of 8 byte hex: a*256^3 + b*256^2 + c*256 + d
ip2integer <- function(ip) sapply(strsplit(ip,".",fixed=TRUE),function(x)sum(as.integer(x)*256^(3:0)))

#add new ip converted to int column
txns['int_ip'] = ip2integer(as.character(txns$ip_address))
ip['int_start'] = ip2integer(as.character(ip$range_start))
ip['int_end'] = ip2integer(as.character(ip$range_end))

# find index of ip_mappings which matches with ip address of ecom_txns - warning: might take time for large data set
indx   <- sapply(txns[['int_ip']],function(x)with(ip[c(3:5)],which(x>=int_start & x <=int_end)))
# create merged data frame
result <- cbind(txns,ip[indx,3])
names(result)
#rename new column to country
colnames(result)[colnames(result)=="ip[indx, 3]"] <- "country"

# move "fraud" target variable to end
result <- result[c(1,2,3,4,5,6,7,8,9,10,12,13,11)]
names(result)
str(result)

# save it in csv for future use
#write.csv(result,file = "merged_ecom_txns.csv")

TFraud <- read.csv('merged_ecom_txns.csv')
TFraud <- TFraud[c(2:14)]

### Task 2: Pre-process (Statistical Analysis)

# check num of observations/variables and data types
str(TFraud)
summary(TFraud)

# pckgs <- c('psych','dplyr','ggplot')
# install.packages(pckgs, dependencies = TRUE)

library('dplyr')
library('psych')

# check if categorial data have unknown/unidentified values
distinct(TFraud, age, .keep_all = FALSE) #dplyr

# filter data for each group, if needed
TFraud_control <- filter(TFraud, group == 'Control')
TFraud_test <- filter(TFraud, group == 'Test')

glimpse(TFraud) # dplyr
describe(TFraud) #psych

# correlation of numeric fields

# first convert categorical features to numeric
TFraudNum <- select(TFraud, "date","source","device","payee","browser","sex","age",
                     "industry_code","group","trial")
directions <- NULL
directions.factor <- NULL
directions <- TFraudNum$group
directions.factor <- factor(directions)
TFraudNum$group <- as.numeric(directions.factor)
# TFraudNum$date <- as.numeric(as.character(as.Date(directions.factor, format = '%m/%d/%Y'), format = '%Y%m%d'))

# write.csv(TFraudNum,file = "merged_trial_results_numMap.csv")

TFraudNum <- read.csv('merged_trial_results_numMap.csv')

str(TFraudNum)
names(TFraudNum)
cor(TFraudNum, TFraudNum$trial, use = 'complete.obs')

# not much information in correlation with mapping, re-try with dummy variables

#install.packages('dummies')
library('dummies')
# create dummies for all factor variables in TFraud
str(TFraud)
names(TFraud)
TFraud.dummy <- dummy.data.frame(select(TFraud, "date","source","device","payee","browser","sex", "age", "industry_code","group","trial"),
                                  names = c("date","source","device","payee","browser","sex","industry_code","group"), 
                                  sep = '.')
names(TFraud.dummy)

cor(TFraud.dummy,TFraud.dummy$trial, use = 'complete.obs')

# now select (n-1) & aggregate dummy variables
newTFraud.dummy <- select(TFraud.dummy, "date.1/10/2017", "date.1/3/2017", "date.1/4/2017", "date.1/5/2017", "date.1/6/2017", "date.1/7/2017", "date.1/8/2017", "date.1/9/2017",
       "source.Email", "source.Facebook", 
       "device.desktop", 
       "payee.Non-Primary", 
       "browser.Android (In-App)", "browser.Chrome", "browser.FireFox", "browser.IE", "browser.iOS (In-App)", "browser.Opera",
       "sex.F", 
       "age",
       "industry_code.AOR", "industry_code.BOR", "industry_code.CIL", "industry_code.DUR", "industry_code.GRT", "industry_code.ICG", "industry_code.LPC", "industry_code.LPK", "industry_code.LPP", "industry_code.MFE", "industry_code.MFG", "industry_code.PGG", "industry_code.PWO", "industry_code.RCA", "industry_code.RGA", "industry_code.SPC", "industry_code.TRV",
       "group.Control", "trial")

newTFraud.dummy <- newTFraud.dummy %>% 
  transmute(date = rowSums(.[grep("date.*", names(.))], na.rm = TRUE),
         source = rowSums(.[grep("source.*", names(.))], na.rm = TRUE),
         device = `device.desktop`,
         payee = `payee.Non-Primary`,
         browser = rowSums(.[grep("browser.*", names(.))], na.rm = TRUE),
         sex = sex.F,
         age = age,
         industry = rowSums(.[grep("industry_code.*", names(.))], na.rm = TRUE),
         group = group.Control,
         trial = trial
         )

distinct(newTFraud.dummy$industry)

# write.csv(newTFraud.dummy,file = "merged_trial_results_dummy.csv")

cor(newTFraud.dummy, newTFraud.dummy$trial, use = 'complete.obs')

### Task 3: Data Visualization

library('ggplot2')

# check skewness of numeric feature
hist(TFraud$age, col = 'red') # histogram

plot(density(filter(TFraud, TFraud$age != 'NA')$age)) # kernel desnity plot

# check distribution of categorical features
barplot(table(TFraud$date), main="date distribution", ylab='Num of dates')
barplot(table(TFraud$source), main="source distribution", ylab='Num of sources')
barplot(table(TFraud$device), main="device distribution", ylab='Num of devices')
barplot(table(TFraud$payee), main="payee distribution", xlab='Type of payee' ,ylab='Num of payees')
barplot(table(TFraud$browser), main="browser distribution", ylab='Num of browsers')
barplot(table(TFraud$sex), main="gender distribution", ylab='Num of genders')
barplot(table(TFraud$industry_code), main="industry_code distribution", ylab='Num of industry_code')
barplot(table(TFraud$group), main="group distribution", ylab='Num of group')
barplot(table(TFraud$trial), main="TV trial distribution", xlab='Trial', ylab='Num of trial')

# check Age distribution across ONE categorical feature - using boxplot
boxplot(TFraud$age ~ TFraud$group, TFraud, xlab = 'Type of group', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$source, TFraud, xlab = 'Type of source', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$device, TFraud, xlab = 'Type of devices', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$payee, TFraud, xlab = 'Type of payee', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$industry_code, TFraud, xlab = 'Type of industry_code', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$sex, TFraud, xlab = 'Type of gender', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$trial, TFraud, xlab = 'Type of trial', ylab = 'Age')

# scatter plot

corFtrial <- round(cor(TFraudNum, use = 'complete.obs'), 5)
corFtrial <- cor(TFraudNum, use = 'complete.obs')
head(corFtrial)

# package reshape is required to melt the correlaion matrix:
library('reshape2')
melted_corFtrial <- melt(corFtrial)
head(corFtrial)

ggplot(melted_corFtrial)

### Task 4: How to fix data quality issues

# transform age to fix skewness - john tukey ladder
head(log(TFraud$age))

hist(log(TFraud$age, 2), col = 'red') # histogram

# convert trial from int to factor
TFraud$trial <- factor(TFraud$trial, levels = c(0,1))
