
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
distinct(TFraud, sex, .keep_all = FALSE) #dplyr
table(TFraud$store)
table(TFraud$browser)
table(TFraud$sex)
table(TFraud$country)
table(TFraud$fraud)

glimpse(TFraud) # dplyr
describe(TFraud) #psych

# Complete cases show us which rows aren't missing values, or have complete cases
complete.cases(TFraud)
# Using brackets to subset: [rows, columns] - let's view the rows with NA's 
# ! means not equal
TFraudNum[!complete.cases(TFraudNum), ]

# correlation of numeric fields

# first convert categorical features to numeric
names(TFraud)
TFraudNum <- select(TFraud, "user_id","signup_datetime","datetime","amount","device_id","store","browser",
                    "sex","age","int_ip","country","fraud")
TFraudNum$store <- as.numeric(factor(TFraudNum$store))
TFraudNum$browser <- as.numeric(factor(TFraudNum$browser))
TFraudNum$sex <- as.numeric(factor(TFraudNum$sex))
TFraudNum$country <- as.numeric(factor(TFraudNum$country))

# date time categorical to numeric
TFraudNum$signup_datetime <- as.numeric(as.character(
  as.Date(TFraudNum$signup_datetime, format = '%Y-%m-%d %H:%M:%S'), format = '%Y%m%d%H%M%S'))
TFraudNum$datetime <- as.numeric(as.character(
  as.Date(TFraudNum$datetime, format = '%Y-%m-%d %H:%M:%S'), format = '%Y%m%d%H%M%S'))

# save in csv file for future use
#write.csv(TFraudNum,file = "merged_ecom_txns_numMap.csv")

TFraudNum <- read.csv('merged_ecom_txns_numMap.csv')
TFraudNum <- TFraudNum[c(2:13)]

str(TFraudNum)
names(TFraudNum)
cor(TFraudNum, TFraudNum$fraud, use = 'complete.obs', method = "pearson")
cor(TFraudNum, TFraudNum$fraud, use = 'complete.obs', method = "kendall")
cor(TFraudNum, TFraudNum$fraud, use = 'complete.obs', method = "spearman")
# features show Weak coorelation with strongest as singup_datetim=0.244 and weakest as device_id close to Zero(0.00002)

# not much information in correlation with mapping, re-try with dummy variables

#install.packages('dummies')
library('dummies')
# create dummies for all factor variables in TFraud
str(TFraud)
names(TFraud)
TFraud.dummy <- dummy.data.frame(select(TFraud, "user_id","signup_datetime","datetime","amount","device_id","store","browser",
                                        "sex","age","int_ip","country","fraud"),
                                  names = c("store","browser","sex","country"),
                                  sep = '.')
names(TFraud.dummy)
str(TFraud.dummy) # Very high dimensional data 214 variables

# now select (n-1) & aggregate dummy variables
newTFraud.dummy <- select(TFraud.dummy, "user_id","signup_datetime","datetime","amount","device_id",
       "store.babies","store.pets",
       "browser.Chrome","browser.FireFox","browser.IE","browser.Opera",
       "sex.F","sex.M", "age","int_ip",
       "country.AE","country.AF","country.AI","country.AL","country.AM","country.AO","country.AR","country.AT","country.AU",
       "country.AZ","country.BA","country.BB","country.BD","country.BE","country.BF","country.BG","country.BH","country.BI",
       "country.BJ","country.BM","country.BN","country.BO","country.BQ","country.BR","country.BS","country.BT","country.BW",
       "country.BY","country.BZ","country.CA","country.CD","country.CG","country.CH","country.CI","country.CL","country.CM",
       "country.CN","country.CO","country.CR","country.CU","country.CV","country.CW","country.CY","country.CZ","country.DE",
       "country.DJ","country.DK","country.DM","country.DO","country.DZ","country.EC","country.EE","country.EG","country.ES",
       "country.ET","country.FI","country.FJ","country.FM","country.FO","country.FR","country.GA","country.GB","country.GD",
       "country.GE","country.GF","country.GH","country.GI","country.GM","country.GP","country.GR","country.GT","country.GU",
       "country.GY","country.HK","country.HN","country.HR","country.HT","country.HU","country.ID","country.IE","country.IL",
       "country.IM","country.IN","country.IO","country.IQ","country.IR","country.IS","country.IT","country.JE","country.JM",
       "country.JO","country.JP","country.KE","country.KG","country.KH","country.KI","country.KN","country.KR","country.KW",
       "country.KY","country.KZ","country.LA","country.LB","country.LC","country.LI","country.LK","country.LS","country.LT",
       "country.LU","country.LV","country.LY","country.MA","country.MC","country.MD","country.ME","country.MG","country.MK",
       "country.MM","country.MN","country.MO","country.MQ","country.MS","country.MT","country.MU","country.MV","country.MW",
       "country.MX","country.MY","country.MZ","country.NC","country.NE","country.NG","country.NI","country.NL","country.NO",
       "country.NP","country.NR","country.NZ","country.OM","country.PA","country.PE","country.PG","country.PH","country.PK",
       "country.PL","country.PR","country.PS","country.PT","country.PW","country.PY","country.QA","country.RE","country.RO",
       "country.RS","country.RU","country.RW","country.SA","country.SC","country.SD","country.SE","country.SG","country.SI",
       "country.SK","country.SM","country.SN","country.SO","country.SS","country.SV","country.SY","country.TH","country.TJ",
       "country.TM","country.TN","country.TR","country.TT","country.TW","country.TZ","country.UA","country.UG","country.US",
       "country.UY","country.UZ","country.VC","country.VE","country.VG","country.VI","country.VN","country.VU","country.XK",
       "country.YE","country.ZA","country.ZM","country.ZW","country.ZZ",
       "fraud")

newTFraud.dummy <- newTFraud.dummy %>% 
  transmute(user_id = user_id,
            signup_datetime = signup_datetime,
            datetime = datetime,
            amount = amount,
            device_id = device_id,
            store = rowSums(.[grep("store.*", names(.))], na.rm = TRUE),
            browser = rowSums(.[grep("browser.*", names(.))], na.rm = TRUE),
            sex = rowSums(.[grep("sex.*", names(.))], na.rm = TRUE),
            age = age,
            int_ip = int_ip,
            country = rowSums(.[grep("country.*", names(.))], na.rm = TRUE),
            fraud = fraud
         )

# save in csv for future use
#write.csv(newTFraud.dummy,file = "merged_ecom_txns_dummy.csv")

newTFraud.dummy <- read.csv('merged_ecom_txns_dummy.csv')
newTFraud.dummy <- newTFraud.dummy[c(2:13)]

str(newTFraud.dummy)

cor(newTFraud.dummy, newTFraud.dummy$fraud, use = 'complete.obs') 
# features show Weak coorelation with strongest as singup_datetim=0.244 and weakest as device_id close to Zero(0.00002)

### Task 3: Data Visualization

library('ggplot2')

# check skewness of numeric feature
hist(TFraud$age, col = 'red') # histogram
hist(TFraud$amount, col = 'red') # histogram

plot(density(filter(TFraud, TFraud$age != 'NA')$age)) # kernel desnity plot

# check distribution of categorical features
barplot(table(TFraud$store), main="store distribution", ylab='Num of stores')
barplot(table(TFraud$browser), main="browser distribution", ylab='Num of browsers')
barplot(table(TFraud$country), main="country distribution", ylab='Num of countries')
barplot(table(TFraud$sex), main="gender distribution", ylab='Num of genders')
barplot(table(TFraud$fraud), main="TV fraud distribution", xlab='fraud', ylab='Num of fraud')

# check Age distribution across ONE categorical feature - using boxplot
boxplot(TFraud$age ~ TFraud$store, TFraud, xlab = 'Type of store', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$browser, TFraud, xlab = 'Type of browser', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$sex, TFraud, xlab = 'Type of gender', ylab = 'Age')
boxplot(TFraud$age ~ TFraud$fraud, TFraud, xlab = 'Type of fraud', ylab = 'Age')
boxplot(TFraud$amount ~ TFraud$store, TFraud, xlab = 'Type of store', ylab = 'Amount')
boxplot(TFraud$amount ~ TFraud$browser, TFraud, xlab = 'Type of browser', ylab = 'Amount')
boxplot(TFraud$amount ~ TFraud$sex, TFraud, xlab = 'Type of gender', ylab = 'Amount')
boxplot(TFraud$amount ~ TFraud$fraud, TFraud, xlab = 'Type of fraud', ylab = 'Amount')

# scatter plot

corFfraud <- round(cor(TFraudNum, use = 'complete.obs'), 5)
corFfraud <- cor(TFraudNum, use = 'complete.obs')
head(corFfraud)

### Task 4: How to fix data quality issues

# transform Amount & age to fix skewness - use john tukey ladder
hist(log(TFraud$age, 10), col = 'red') # histogram
hist(log(TFraud$amount, 10), col = 'red') # histogram

# convert fraud from int to factor
TFraudNum$fraud <- factor(TFraudNum$fraud, levels = c(0,1))
newTFraud.dummy$fraud <- factor(newTFraud.dummy$fraud, levels = c(0,1))

# TV 'fraud' is unbalanced
# either use imbalance data with F1 Measure of Accuracy or balance the data using over/under sampling

### Task 5: Machine Learning/Predictive Analytics

# First split the data
install.packages("caTools")
library('caTools')

set.seed(123)

split <- sample.split(TFraudNum$fraud, SplitRatio = 0.80)
training_set <- subset(TFraudNum, split == TRUE)
test_set <- subset(TFraudNum, split == FALSE)

# repeat above steps for dummy dataset
# split <- sample.split(newTFraud.dummy$fraud, SplitRatio = 0.80)
# training_set_dummy <- subset(newTFraud.dummy, split == TRUE)
# test_set_dummy <- subset(newTFraud.dummy, split == FALSE)

# Build first Model - Decition Tree
library('rpart')

DTmodel <- rpart(formula = fraud ~ .,
                 data = training_set,
                 method = "class")
                 #control = rpart.control(maxdepth = 10))
DTmodel
#######################################################################################################
### Predicting the Test set results
#######################################################################################################
DTpred = predict(DTmodel, type = 'class', newdata = test_set[-12]) 

#######################################################################################################
### Making the Confusion Matrix
#######################################################################################################

DTcm = table(test_set[, 12], DTpred)

DTcm  # Type I error - 93, Type II error - 1314

n = sum(DTcm) # number of instances
nc = nrow(DTcm) # number of classes
diag = diag(DTcm) # number of correctly classified instances per class 
rowsums = apply(DTcm, 1, sum) # number of instances per class
colsums = apply(DTcm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

########################## Calculating the Accuracy  ##################################################
Accuracy = sum(diag(DTcm))/sum(DTcm) # 95%
Precision = diag/colsums
Recall = diag / rowsums 
F1_Score = 2 * Precision * Recall / (Precision + Recall) 

########################## Visualize the Decision Tree  ##################################################
install.packages("rpart.plot")
library('rpart.plot')
rpart.plot(DTmodel, box.palette = "RdBu", shadow.col = "gray", nn=TRUE)

# Build second Model - Naive Bayes


# Build third Model - K Neares neighbor


# Build fourth Model - Logistic regression
#######################################################################################################
### Feature Scaling
#######################################################################################################

training_set[-3] = scale(training_set[-12]) 
# prediction boundary
test_set[-3] = scale(test_set[-9])

#######################################################################################################
### Fitting Logistic Regression to the Training set
#######################################################################################################

LogisticModel = glm(formula = Churned ~ .,
                    family = binomial,
                    data = training_set)
LogisticModel
#######################################################################################################
### Predicting the Test set results
#######################################################################################################
prob_pred = predict(LogisticModel, type = 'response', newdata = test_set[-9]) 

prob_pred

glm_pred = ifelse(prob_pred > 0.5, 1, 0)  # Transform probabilities into 1 OR 0

glm_pred

#######################################################################################################
### Making the Confusion Matrix
#######################################################################################################

glmcm = table(test_set[, 9], glm_pred)

glmcm  # 14 incorrect predictions