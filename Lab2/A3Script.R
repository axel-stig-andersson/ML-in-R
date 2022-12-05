# Assignment 3
library(dplyr)
library(caret)
data <- read.csv("Data/communities.csv")
data_wo_VCPP <- data%>%select(-ViolentCrimesPerPop)

scaler <- preProcess(data_wo_VCPP)
scaled_data <- cbind(predict(scaler, newdata = data_wo_VCPP),data$ViolentCrimesPerPop)
cov = cov()
#---------------------------------------------------------------------------------
#------------------------------------Task 2---------------------------------------
#---------------------------------------------------------------------------------
