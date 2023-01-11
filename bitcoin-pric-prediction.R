# Szymon Bartoszewicz
# Financial Data Science
# UNIPV 2023

# The dataset consists of daily (without weekends & holidays) prices in U.S.
# dollars of: bitcoin, stock prices of the leading global manufacturers of GPUs,
# Big Tech companies, prominent payment-processing corporations, and Tesla, Inc.
# stock prices. It comes from Google Finance.

##############################################################################
##############################################################################
##############################################################################
# 
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
btc <- read_excel("/Users/simon/Documents/classes/Financial data science/final pres/Bartoszewicz-btc.xlsx")

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

# This is one of the methods of transforming skewed data. (log(x)-log(x-1))
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

# Creating a dataset after winsorization
data3      <- sapply(data2, rmOutlier)

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

##############################################################################
##############################################################################
##############################################################################
# 1st model - GPUs manufacturers
##############################################################################
##############################################################################
##############################################################################

# Multiple linear regression on the training dataset
fit1     <- lm(btcusd ~ amd + nvda, data_train)
summary(fit1)

# Performing a stepwise linear regression 
fit_step_f <- step(fit1, direction='forward')
summary(fit_step_f)

fit_step_b <- step(fit1, direction='backward')
summary(fit_step_b)

fit_step   <- step(fit1, direction='both')
summary(fit_step)
# plot(fit_step)



