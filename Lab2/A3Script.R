# Assignment 3
library(dplyr)
library(caret)
#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------
data <- read.csv("Data/communities.csv", stringsAsFactors = T)
data_wo_VCPP <- data%>%select(-ViolentCrimesPerPop)

scaler <- preProcess(data_wo_VCPP)
scaled_data <- as.data.frame(cbind(predict(scaler, newdata = data_wo_VCPP),data$ViolentCrimesPerPop))
covariance = cov(scaled_data)
eigen_vals <- eigen(covariance, only.values = T)
#---------------------------------------------------------------------------------
#------------------------------------Task 2---------------------------------------
#---------------------------------------------------------------------------------

