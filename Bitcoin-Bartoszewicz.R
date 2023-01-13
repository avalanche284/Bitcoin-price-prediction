# Szymon Bartoszewicz
# Financial Data Science
# UNIPV 2023

# The dataset consists of daily (without weekends & holidays) prices in U.S.
# dollars of:bitcoin, stock prices of the leading global manufacturers of GPUs,
# the Big Tech companies, two prominent payment-processing corporations, and
# Tesla, Inc. stock prices. It comes from Google Finance.

##############################################################################
##############################################################################
##############################################################################
# Descriptive statistics
##############################################################################
##############################################################################
##############################################################################

# Cleaning the environment
rm(list=ls())

# Loading packages
library(DescTools)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(pastecs)
library(corrplot)
library(ppcor)
library(readxl)
library(forecast)

# Importing the dataset
btc <- read_excel("/Users/simon/github_repos/Bitcoin-price-prediction/dataset.xlsx")

# Creating a copy of the dataset on which operations are performed
data <- btc

# Investigating the dataset
# dimensions of the dataset
dim(data)

# names of the columns
colnames(data) 

# showing mean of every columns besides the date
sapply(data[-1], mean)

# summary
summary(data)
str(data)

plot(data[-1])

stat.desc(data$btcusd)

# histogram
hist(data$btcusd) 
boxplot(data$btcusd, main = "Boxplot of the Bitcoin price (Source: Google Finance)")

# Plotting btc price change over time
ggplot(data = data, aes(Date, btcusd)) + geom_line(colour='gold')

# Comparison --> Comparing the movement in price
ggplot(data, aes(Date)) + 
  geom_line(aes(y = btcusd, colour = "btcusd")) + 
  geom_line(aes(y = amd, colour = "amd")) + 
  geom_line(aes(y = nvda, colour = "nvda")) + 
  
  geom_line(aes(y = meta, colour = "meta")) + 
  geom_line(aes(y = aapl, colour = "aapl")) + 
  geom_line(aes(y = googl, colour = "googl")) + 
  geom_line(aes(y = nflx, colour = "nflx")) + 
  geom_line(aes(y = msft, colour = "msft")) + 
  geom_line(aes(y = amzn, colour = "amzn")) + 
  
  geom_line(aes(y = ma, colour = "ma")) + 
  geom_line(aes(y = v, colour = "v")) + 
  geom_line(aes(y = dfs, colour = "dfs")) + 
  geom_line(aes(y = axp, colour = "axp")) + 
  
  geom_line(aes(y = tsla, colour = "tsla")) 

# Getting rid of date column
data1<-data[-1]
  
# # Creating data.frames of the main four groups
# gpu <- data.frame(data1$nvda, data1$amd)
# bigtech <- data.frame(data1$meta, data1$aapl, data1$amzn, data1$googl, data1$msft, data1$nflx)
# cards <- data.frame(data1$v, data1$ma, data1$dfs, data1$axp)
# tesla <- data.frame(data1$tsla)

### Correlations

# # Checking correlations btc ~ groups
# # (btc ~ nvda + amd)/2
# corgpu <- mean(cor(data.frame(data1$btcusd, data1$nvda)), cor(data.frame(data1$btcusd, data1$amd)))
# corgpu
# corgpu <- mean(cor(data1$btcusd, gpu))
# corgpu
# 
# # Checking correlations in groups # hey. it doesnt make sense
# corgpu <- cor(data1$btcusd, gpu)
# corgpu

# Visualizing the correlation plot
correlations <- cor(data1)
correlations
corrplot(correlations, method="circle", type = "lower", diag = FALSE)
corrplot(correlations, method="number", type = "lower", diag = FALSE)

##############################################################################
##############################################################################
##############################################################################
# Linear Regression Models
##############################################################################
##############################################################################
##############################################################################

# Cleaning the environment
rm(list=ls())

# Loading packages
library(DescTools)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(pastecs)
library(corrplot)
library(ppcor)
library(readxl)
library(forecast)

# Importing the dataset
btc <- read_excel("/Users/simon/github_repos/Bitcoin-price-prediction/dataset.xlsx")

# Creating a copy of the dataset on which operations are performed
data <- btc

# Getting rid of date column
data1<-data[-1]

# Creating a separate dataset with returns instead of prices (log(x)-log(x-1))
data2 <- as.data.frame(sapply(data1, function(x) diff(log(x), lag=1)))
head(data2)

# Running a simple linear regression y=a+bx [btcusd on nvda]
model1 <- lm(btcusd ~ nvda, data = data2)
summary(model1) # poor model

# Running a simple linear regression y=a+bx [btcusd on amd]
model2 <- lm(btcusd ~ amd, data = data2)
summary(model2) # poor model

# Running a simple linear regression y=a+bx [btcusd on googl] # the strongest correlation
model3 <- lm(btcusd ~ googl, data = data2)
summary(model3) # poor model

# Running a simple linear regression y=a+bx [btcusd on tsla]
model4 <- lm(btcusd ~ tsla, data = data2)
summary(model4) # poor model

# Running a multiple linear regression y=a+bx [btcusd on all]
model5 <- lm(btcusd ~ ., data = data2)
summary(model5)

coeff <- model5$coefficients
# What's the reason behing it? 
# Get and plot residuals
res <- model5$residuals
# res <- residuals(model5)
plot(res, type='l')

# Convert to DataFrame for gglpot
res <- as.data.frame(res)

# Check residuals' distribution 
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5, binwidth=0.003)

# model diagnostics
# plot(model5)

# Getting fitted values 
fit <- model5$fitted.values
# fit <- fitted.values(model5)

date_returns <- data$Date[2:nrow(data)]

results <- cbind.data.frame(date_returns, data2$btcusd, fit)
colnames(results)<-c('Date','Observed','Fitted')

# Plotting observed vs fitted values
ggplot(results, aes(Date)) + 
  geom_line(aes(y = Observed, colour = "Observed")) + 
  geom_line(aes(y = Fitted, colour = "Fitted")) +
  ylab('Returns') +
  ggtitle('bcusd') + theme(plot.title = element_text(hjust = 0.5))

# Evaluating model prediction 
# MSE method for evaluating model5 prediction (mean squared error)
mse <- mean((results$Fitted-results$Observed)^2)
print(mse)

# Root mean squared error
mse^0.5

# R-Squared Value for the model5
SSE = sum((results$Fitted - results$Observed)^2)
SST = sum( (results$Observed - mean(results$Observed) )^2)
R2 = 1 - SSE/SST
R2

# Splittin the sample in two, to make out-of-sample predictions ==using past data to make forecast in the future
data2_in  <- data2[1:1253,]
data2_out <- data2[1253:nrow(data2),]

# Estimate the model on the first subsample
model_out <- lm(btcusd ~., data = data2_in)

# Appling the model to the second subsample
fit_out <- predict(model_out, data2_out)

date_returns_out <- date_returns[1253:nrow(data2)]

results_out <- cbind.data.frame(date_returns_out, data2_out$btcusd, fit_out)
colnames(results_out)<-c('Date','Observed','Predicted')

# Calculate RMSE for the out-of-sample predictions
plot(data2_out$btcusd, fit_out)
rmse <- sqrt(mean((results_out$Predicted-results_out$Observed)^2))
print(rmse)

##############################################################################
##############################################################################
##############################################################################
# Linear Model Selection 
##############################################################################
##############################################################################
##############################################################################

# Cleaning the environment
rm(list=ls())

# Loading packages
library(DescTools)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(pastecs)
library(corrplot)
library(ppcor)
library(readxl)
library(forecast)

# Importing the dataset
btc <- read_excel("/Users/simon/github_repos/Bitcoin-price-prediction/dataset.xlsx")

# Creating a copy of the dataset on which operations are performed
data <- btc

# Investigating the dataset
dim(data)
colnames(data) 
sapply(data[-1], mean)
summary(data[-1])
str(data)
plot(data[-1])
stat.desc(data$btcusd)
hist(data$btcusd) 
boxplot(data$btcusd, main = "Boxplot of the Bitcoin price (Source: Google Finance)")

# Plotting btc price change over time
ggplot(data = data, aes(Date, btcusd)) + geom_line(colour='gold')

# Comparison --> Comparing the movement in price
ggplot(data, aes(Date)) + 
  geom_line(aes(y = btcusd, colour = "btcusd")) + 
  geom_line(aes(y = amd, colour = "amd")) + 
  geom_line(aes(y = nvda, colour = "nvda")) + 
  
  geom_line(aes(y = meta, colour = "meta")) + 
  geom_line(aes(y = aapl, colour = "aapl")) + 
  geom_line(aes(y = googl, colour = "googl")) + 
  geom_line(aes(y = nflx, colour = "nflx")) + 
  geom_line(aes(y = msft, colour = "msft")) + 
  geom_line(aes(y = amzn, colour = "amzn")) + 
  
  geom_line(aes(y = ma, colour = "ma")) + 
  geom_line(aes(y = v, colour = "v")) + 
  geom_line(aes(y = dfs, colour = "dfs")) + 
  geom_line(aes(y = axp, colour = "axp")) + 
  
  geom_line(aes(y = tsla, colour = "tsla")) 

# Getting rid of "Date" column for the analysis
data1<-data[-1]

# Creating a separate dataset with returns instead of prices (log(x)-log(x-1))
# This is one of the methods of transforming skewed data.
data2 <- as.data.frame(sapply(data1, function(x) diff(log(x), lag=1)))
head(data2)
dim(data2)
head(data2)
summary(data2)
str(data2)

# Checking the distribution of btcusd in the sample
boxplot(data2$btcusd, main = "btcusd")

# Dealing with (possible) outliers: winsorization
rmOutlier <- function(x){
  low  <- quantile(x, 0.05, na.rm = T)
  high <- quantile(x, 0.95, na.rm = T)
  out <- ifelse(x > high, high,ifelse(x < low, low, x))
  out }

data3      <- sapply(data2, rmOutlier)

# Visualizing the correlation plot
correlations1 <- cor(data1)
correlations1
corrplot(correlations1, method="circle", type = "lower", diag = FALSE)
corrplot(correlations1, method="number", type = "lower", diag = FALSE)

# data2 is data after transforming skeweness (logs)
correlations2 <- cor(data2)
correlations2
corrplot(correlations2, method="circle", type = "lower", diag = FALSE)
corrplot(correlations2, method="number", type = "lower", diag = FALSE)

# data3 is after winsorization
correlations3 <- cor(data3)
correlations3
corrplot(correlations3, method="circle", type = "lower", diag = FALSE)
corrplot(correlations3, method="number", type = "lower", diag = FALSE)

# Checking partial correlations by using the pcor function 
p_correlations3 <- pcor(data3)
pcor_mat3       <- p_correlations3$estimate

# Visualizing the partial correlation plot
corrplot(pcor_mat3, method="circle", type = "lower", diag = FALSE)
corrplot(pcor_mat3, method="number", type = "lower", diag = FALSE) 

# Setting a random seed so that results can be reproduced
set.seed(1000)

# Splitting the dataset into training and testing samples 
n_train <- round(nrow(data3)*0.8) 

data_all   <- data3[sample(nrow(data3)), ]          
data_train <- as.data.frame(data3[1:n_train, ])
data_test <- as.data.frame(data3[(n_train+1):nrow(data3), ])
###########################################################
# LINEAR REGRESSION
# Multiple linear regression on the training dataset
fit1     <- lm(btcusd ~ ., data_train)
summary(fit1)

# Performing a stepwise linear regression 
fit_step_f <- step(fit1, direction='forward')
summary(fit_step_f)

fit_step_b <- step(fit1, direction='backward')
summary(fit_step_b)

fit_step   <- step(fit1, direction='both')
summary(fit_step)
# plot(fit_step)

# Getting residuals and performing an F-test to compare the full model with the reduced one
res      <- fit_step$residuals
res_full <- fit1$residuals

SSE_red  <- sum(res^2)
SSE_full <- sum(res_full^2)

p <- length(coefficients(fit_step))-1 
k <- length(coefficients(fit1)) - length(coefficients(fit_step)) 
N <- nrow(data_train)

f_stat_num <- (SSE_red-SSE_full)/k
f_stat_den <- SSE_full/(N-p-k-1)
f_stat     <- f_stat_num/f_stat_den
f_pvalue   <- 1-pf(f_stat, df1=k, df2=N-p-k-1)

# ANOVA for model comparison
m0 <- lm(btcusd ~ 1, data_train) #model with no predictors
m1 <- fit_step
anova(m0,m1)
m2 <- fit1
anova(m1,m2)

# Making predictions on the test dataset
predictions       <- predict(fit_step,data_test)
results           <- cbind(data_test$btcusd,predictions) 
colnames(results) <- c('Real','Predicted')
results           <- as.data.frame(results)

# Calculating error measures
# MSE
mse  <- mean((results$Real-results$Predicted)^2)
print(mse)

# RMSE
rmse     <- sqrt(mse)
print(rmse)

# MAE
mae <- mean(abs(results$Real-results$Predicted))
print(mae)

# Diebold-Mariano test
dmar<-dm.test(fit_step$residuals,fit1$residuals,alternative="two.sided")
dmar
