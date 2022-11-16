# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 1 ------------------------------------------
# ----------------------------------------------------------------------------------------
  
# Reading the data
data <- read.csv("data/parkinsons.csv")

# Dividing in to training and test
library(caret)
library(dplyr)

n=dim(data)[1]
set.seed(12345) 
train_index=sample(1:n, floor(n*0.6))  

train = data[train_index,]
test_index=setdiff(1:n, train_index)
set.seed(12345)
test = data[test_index,]

scaler <- preProcess(train)
train_scaled <- predict(scaler, train)
test_scaled <- predict(scaler, test)

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 2 ------------------------------------------
# ----------------------------------------------------------------------------------------

features=train_scaled%>%select(motor_UPDRS, Jitter...:PPE) 

lr1 <- lm(motor_UPDRS~., features)
summary(lr1)


# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 3 ------------------------------------------
# ----------------------------------------------------------------------------------------
N <- nrow(train_scaled)
Y <- as.matrix(train_scaled$motor_UPDRS)
X <- as.matrix(train_scaled%>%select(Jitter...:PPE))
loglikelyhood = function(theta){
  sigma <- tail(theta,n=1)
  
  return(-N/2*log(2*pi*sigma^2) - (1 / (2*(sigma^2)))* (t(Y - X %*% theta[-17])) %*% (Y - X%*%theta[-17]))  
}

Ridge <-  function(theta, lambda) {
  return(lambda*sum(theta[-17]^2) - loglikelyhood(theta))
} 
RidgeOpt <- function(theta, lambda) {
  return(optim(theta, fn = Ridge, lambda=lambda, method = "BFGS")) 
}

df <- function(lambda) {
  I <- as.matrix(diag(16))
  return(sum(diag((X%*%(t(X)%*%X + lambda*I)^(-1))%*%t(X))))
}

#Testing for different lambda values
theta.opt.lambda1 <- RidgeOpt(theta=rep(1,17), lambda = 1)
theta.opt.lambda100 <- RidgeOpt(theta=rep(1,17), lambda = 100)
theta.opt.lambda1000 <- RidgeOpt(theta=rep(1,17), lambda = 1000)


# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 4 ------------------------------------------
# ----------------------------------------------------------------------------------------
# Multiplying each row in the data (x-values) with the theta values, to obtain a prediction (scaled)
pred.train.l1 <- as.matrix(train_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda1$par[-17])
pred.train.l100 <- as.matrix(train_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda100$par[-17])
pred.train.l1000 <- as.matrix(train_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda1000$par[-17]) 

pred.test.l1 <- as.matrix(test_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda1$par[-17])
pred.test.l100 <- as.matrix(test_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda100$par[-17])
pred.test.l1000 <- as.matrix(test_scaled%>%select(Jitter...:PPE)) %*% as.matrix(theta.opt.lambda1000$par[-17])

# Computing Mean Square Errors
mse.train.opt.1 <- mean((train_scaled$motor_UPDRS-pred.train.l1)^2)
mse.train.opt.100 <- mean((train_scaled$motor_UPDRS-pred.train.l100)^2)
mse.train.opt.1000 <- mean((train_scaled$motor_UPDRS-pred.train.l1000)^2)
mse.test.opt.1 <- mean((test_scaled$motor_UPDRS-pred.test.l1)^2)
mse.test.opt.100 <- mean((test_scaled$motor_UPDRS-pred.test.l100)^2)
mse.test.opt.1000 <- mean((test_scaled$motor_UPDRS-pred.test.l1000)^2) 

# Degrees of freedom
df.1 <- df(100) 
df.100 <- df(100)
df.1000 <- df(1000)
