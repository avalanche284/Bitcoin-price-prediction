# Szymon Bartoszewicz
# Financial Data Science
# UNIPV 2023

# The dataset consists of daily (without weekends & holidays) prices in U.S.
# dollars of: bitcoin, stock prices of the leading global manufacturers of GPUs,
# Big Tech companies, payment-processing corporations, and Tesla, Inc.stock
# prices. It comes from Google Finance.

##############################################################################
##############################################################################
##############################################################################
# Descriptive statistics -- investigation of the dataset
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

# Comparison -- Comparing the movement in price
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

##############################################################################
# Getting rid of "Date" column for the analysis
data1<-data[-1]
##############################################################################
# Transforming skewed data (log(x)-log(x-1))
data2 <- as.data.frame(sapply(data1, function(x) diff(log(x), lag=1)))
head(data2)
dim(data2)
head(data2)
summary(data2)
str(data2)

# Checking the distribution of btcusd in the sample
boxplot(data2$btcusd, main = "btcusd")

##############################################################################
# Dealing with (possible) outliers: winsorization
rmOutlier <- function(x){
  low <- quantile(x, 0.05, na.rm = T)
  high <- quantile(x, 0.95, na.rm = T)
  out <- ifelse(x > high, high,ifelse(x < low, low, x))
  out }
##############################################################################
# Creating a dataset after winsorization
data3 <- sapply(data2, rmOutlier)

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

data4 <- as.data.frame(data3)

# Setting a random seed so that results can be reproduced
set.seed(1000)
##############################################################################
##############################################################################
##############################################################################
# 1st model -- bitcoin price against all all other features
##############################################################################
##############################################################################
##############################################################################

# Multiple linear regression on the training dataset
fit_1 <- lm(btcusd ~ ., data4)
summary(fit_1)

# Performing a stepwise linear regression 
fit_1_step_f <- step(fit_1, direction='forward')
summary(fit_1_step_f)

fit_1_step_b <- step(fit_1, direction='backward')
summary(fit_1_step_b)

fit_1_step <- step(fit_1, direction='both')
summary(fit_1_step) # showing the best regression
# plot(fit_1_step)

# PREDICTION on the same dataset

data4_test <- data4[round(0.8*nrow(data4)):nrow(data4),]

# Making predictions on the data4_test
predictions_1 <- predict(fit_1_step,data4_test)
results_1 <- cbind(data4_test$btcusd,predictions_1) 
colnames(results_1) <- c('Real','Predicted')
results_1 <- as.data.frame(results_1)

# Calculating error measures
# MSE
mse_1  <- mean((results_1$Real-results_1$Predicted)^2)
print(mse_1)

# RMSE
rmse_1 <- sqrt(mse_1)
print(rmse_1)

# MAE
mae_1 <- mean(abs(results_1$Real-results_1$Predicted))
print(mae_1)

# Diebold-Mariano test
dmar<-dm.test(fit_1_step$residuals,fit_1$residuals,alternative="two.sided")
dmar


##############################################################################
##############################################################################
##############################################################################
# 2nd model -- bitcoin price against groups
##############################################################################
##############################################################################
##############################################################################

# Creating a dataset containing average stock prices as follows:
# GPU <- amd + nvda
# Tech <- googl + aapl + meta + amzn + nflx + msft
# Cards <- v + ma + axp + dfs
# Tsla <- tsla
# data1 a dataset containing prics before eliminating skewness
GPU <- data.frame((data$nvda + data$amd)/2)
TECH <- data.frame((data$meta + data$aapl + data$amzn + data$nflx + data$googl + data$msft)/6)
CARD <- data.frame((data$v + data$ma + data$dfs + data$axp)/4)
TSLA <- data.frame(data$tsla)
##############################################################################
data_gr <- cbind(data$Date, data$btcusd, GPU, TECH, CARD, TSLA)
colnames(data_gr) <- c("Date", "btcusd", "GPU", "TECH", "CARD", "TSLA")

# Here can be done an analysis of some sort e.g. movement in the price for instance
summary(data_gr)
hist(data_gr)
hist(data_gr$btcusd)
hist(data_gr$GPU)
hist(data_gr$TECH)
hist(data_gr$CARD)
hist(data_gr$TSLA)
##############################################################################
# now the new dataset can be tansformed to remove skewweness
data_gr1<-data_gr[-1]

# Transforming skewed data (log(x)-log(x-1))
data_gr2 <- as.data.frame(sapply(data_gr1, function(x) diff(log(x), lag=1)))
head(data_gr2)
dim(data_gr2)
head(data_gr2)
summary(data_gr2)
str(data_gr2)
hist(data_gr2)
hist(data_gr2$btcusd)
hist(data_gr2$GPU)
hist(data_gr2$TECH)
hist(data_gr2$CARD)
hist(data_gr2$TSLA)

# Checking the distribution of btcusd in the sample
boxplot(data_gr2$btcusd, main = "btcusd")
##############################################################################
# Dealing with (possible) outliers: winsorization
rmOutlier <- function(x){
  low  <- quantile(x, 0.05, na.rm = T)
  high <- quantile(x, 0.95, na.rm = T)
  out <- ifelse(x > high, high,ifelse(x < low, low, x))
  out }

##############################################################################
# Creating a dataset after winsorization
data_gr3 <- sapply(data_gr2, rmOutlier)

# Checking correlations
correlations_3gr <- cor(data_gr3)
correlations_3gr
corrplot(correlations_3gr, method="circle", type = "lower", diag = FALSE)
corrplot(correlations_3gr, method="number", type = "lower", diag = FALSE)

# Checking partial correlations by using the pcor function 
p_correlations_3gr <- pcor(data_gr3)
pcor_mat_3gr <- p_correlations_3gr$estimate

# Visualizing the partial correlation plot
corrplot(pcor_mat_3gr, method="circle", type = "lower", diag = FALSE)
corrplot(pcor_mat_3gr, method="number", type = "lower", diag = FALSE) 

##############################################################################
data_gr4 <- as.data.frame(data_gr3)

# Multiple linear regression on the training dataset
fit_gr <- lm(btcusd ~ ., data_gr4)
summary(fit_gr)

# Performing a stepwise linear regression 
fit_gr_step_f <- step(fit_gr, direction='forward')
summary(fit_gr_step_f)

fit_gr_step_b <- step(fit_gr, direction='backward')
summary(fit_gr_step_b)

fit_gr_step <- step(fit_gr, direction='both')
summary(fit_gr_step) # showing the best regression
# plot(fit_gr_step)

# PREDICTION on the same dataset

data_gr4_test <- data_gr4[round(0.8*nrow(data_gr4)):nrow(data_gr4),]

# Making predictions on the data_gr4_test
predictions_gr <- predict(fit_gr_step,data_gr4_test)
results_gr <- cbind(data_gr4_test$btcusd,predictions_gr) 
colnames(results_gr) <- c('Real','Predicted')
results_gr <- as.data.frame(results_gr)

# Calculating error measures
# MSE
mse_gr <- mean((results_gr$Real-results_gr$Predicted)^2)
print(mse_gr)

# RMSE
rmse_gr <- sqrt(mse_gr)
print(rmse_gr)

# MAE
mae_gr <- mean(abs(results_gr$Real-results_gr$Predicted))
print(mae_gr)

# Diebold-Mariano test
dmar_gr <-dm.test(fit_gr_step$residuals,fit_gr$residuals,alternative="two.sided")
dmar_gr


