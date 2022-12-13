# Assignment 2
# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes
library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# filter0 fits the model to the training data, and predicts the validation data. However, it 
# uses the C-value from minimizing the results of the error from previous filter, which was also 
# fitted to the training data. For filter3, same logic applies to the fact that it is fitted to all
# data, which makes it bias. Hence we choose the lowest error of filter1 and filter2, which is filter2.

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# Since we chose filter2, the generalization error is err2.

# 3. Implementation of SVM predictions.


library(pracma)

sv<-alphaindex(filter3)[[1]]
support_vectors <- spam[sv, -58]
co<-coef(filter3)[[1]]
inte<- - b(filter3)

kernel_func <- rbfdot(0.05)
k<-NULL
dot.products <- c()

for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2<-NULL
  for(j in 1:length(sv)){
    k2<- unlist(support_vectors[j,])
    sample = unlist(spam[i, -58])
    dot.prod <- kernel_func(sample, k2)
    dot.products = c(dot.products, dot.prod)
  }
  start.index = 1 + length(sv) * (i-1)
  end.index = length(sv) * i
  prediction = co%*%dot.products[start.index:end.index] + inte
  k<-c(k, prediction)
}
k
predict(filter3,spam[1:10,-58], type = "decision")

