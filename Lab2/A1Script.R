# Assignment 1 - Explicit Regularization
data <- read.csv("Data/tecator.csv") 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,] 
attach(data)

#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------
library(dplyr)
features <- train%>%select(Channel1:Fat)
lin.reg.1 <- lm(Fat~., data = features) 
pred.train <- predict(object=lin.reg.1, newdata = train, type = "response")
pred.test <- predict(object = lin.reg.1, newdata = test, type = "response")
summary(lin.reg.1)

mse_train <- mean((train$Fat-pred.train)^2)
mse_test <- mean((test$Fat-pred.test)^2)

#---------------------------------------------------------------------------------
#------------------------------------Task 2 and 3---------------------------------------
#---------------------------------------------------------------------------------
# install.packages("glmnet")
library(glmnet)
features <- train%>%select(Channel1:Channel100)
lasso_coefs <- c()
lambdas <- seq(from=0.01, to=0.99, by=0.01)

for (i in lambdas) {
  nr_of_coef = 0
  lassotime <- glmnet(x=features, y=train$Fat, alpha = 1, family="gaussian", lambda = i)
  coefs <- coef(lassotime)
  for (j in 2:101) {
    if (coef(lassotime)[j] != 0) {
      nr_of_coef <- nr_of_coef + 1
    }
  }
  lasso_coefs <- c(lasso_coefs, nr_of_coef)
}
plot(log(lambdas), lasso_coefs, ylab = "Number of features", col="red", pch=c(16))


#---------------------------------------------------------------------------------
#------------------------------------Task 4---------------------------------------
#---------------------------------------------------------------------------------
library(glmnet)
features <- train%>%select(Channel1:Channel100)
ridge_coefs <- c()
lambdas <- seq(from=0.01, to=0.99, by=0.01)

for (i in lambdas) {
  nr_of_coef_2 = 0
  ridgetime <- glmnet(x=features, y=train$Fat, alpha = 0, family="gaussian", lambda = i)
  coefs2 <- coef(ridgetime)
  for (j in 2:101) {
    if (coef(ridgetime)[j] != 0) {
      nr_of_coef_2 <- nr_of_coef_2 + 1
    }
  }
 ridge_coefs <- c(ridge_coefs, nr_of_coef_2)
}
plot(log(lambdas), ridge_coefs, ylab = "Number of features", col="red", pch=c(16))


#---------------------------------------------------------------------------------
#------------------------------------Task 5---------------------------------------
#---------------------------------------------------------------------------------
features <- train%>%select(Channel1:Channel100)
crossval1=cv.glmnet(as.matrix(x=features), y = train$Fat, alpha=1,family="gaussian")
plot(crossval1)

# Best lambda
crossval1[["lambda"]][51]
best_lambda <- crossval1[["lambda.min"]]

#Highest "acceptable" lambda
crossval1[["lambda"]][46]
crossval1[["lambda.1se"]]
# Nr of variables chosen when lambda is optimal = 8
crossval1[["nzero"]][51]

# Predictions
x_vals <- as.matrix(test%>%select(Channel1:Channel100))
pred_test <- predict(crossval1, newx = x_vals, s=best_lambda)

plot(test$Fat, col="red", type = "l", xlab)
lines(pred_test, col="green")
legend("bottomright", legend = c("Actual Test", "Prediction"), col = c("red", "green"),pch=1)


 
