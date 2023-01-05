# Assignment 3

library(neuralnet)
#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------

set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, sin=sin(Var))
train <- mydata[1:25,] # Training
test <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
winit <- runif(10,-1,1)
# h <- rep(10,10)
  nn <- neuralnet(sin~., data=train, hidden=10, threshold=0.01, startweights = winit, act.fct = "logistic")
    # Plot of the training data (black), test data (blue), and predictions (red)
    plot(train, cex=4, col="green")
    points(test, col = "blue", cex=1)
    points(test[,1],predict(nn,test), col="red", cex=1)
    legend("bottomright", legend = c("Train data", "Test data", "Predictions"),
           pch = c(cex=1, cex=1, cex=1),
                   col=c("green", "blue", "red"))  
  
    
    
#---------------------------------------------------------------------------------
#------------------------------------Task 2---------------------------------------
#---------------------------------------------------------------------------------

# Creating functions to input as act.fct in the neuralnets below.
linear <- function(x) x
ReLU <- function(x) ifelse(x>0, x, 0)
softplus <- function(x) log(1 + exp(x))

# fitting the models
nn.linear <- neuralnet(sin~., data=train, hidden=10, threshold=0.01, startweights = winit, act.fct = linear)  
nn.ReLU <- neuralnet(sin~., data=train, hidden=10, threshold=0.01, startweights = winit, act.fct = ReLU)
nn.softplus <- neuralnet(sin~., data=train, hidden=10, threshold=0.01, startweights = winit, act.fct = softplus)

# plotting
plot(train, cex=4, col="green", ylim=c(-1.5,1.5))
points(test, col = "blue", cex=1)
points(test[,1],predict(nn.ReLU,test), col="red", cex=1)
legend("bottomright", legend = c("Train data", "Test data", "Predictions"),
       pch = c(cex=1, cex=1, cex=1),
       col=c("green", "blue", "red"))  

#---------------------------------------------------------------------------------
#------------------------------------Task 3---------------------------------------
#---------------------------------------------------------------------------------
set.seed(1234567890)
Var <- runif(500, 0, 50)
mydata2 <- data.frame(Var, sin=sin(Var))
plot(mydata2, cex=2, ylim=c(-10,10), col="blue")
points(mydata2[,1],predict(nn,mydata2), col="red", cex=1) 
legend("topright", legend = c("Data", "Predictions"),
       pch = c(cex=1, cex=1),
       col=c("blue", "red"))  


#---------------------------------------------------------------------------------
#------------------------------------Task 4---------------------------------------
#---------------------------------------------------------------------------------

# Different startweights due to runif. Therefore, different weights from other who did it.
nn[["weights"]]
nn$weights
(nn$weights[[1]][[2]][1])+(nn$weights[[1]][[2]][7])+ (nn$weights[[1]][[2]][8])+(nn$weights[[1]][[2]][10])+(nn$weights[[1]][[2]][11])
# since sigmoid conceges to 0 or 1 we only care bout positive input weight indices.
#---------------------------------------------------------------------------------
#------------------------------------Task 5---------------------------------------
#---------------------------------------------------------------------------------
  set.seed(1234567890)
  Var <- runif(500, 0, 10)
  mydata3 <- data.frame(sin=sin(Var), Var)
  nn_revert <- neuralnet(Var~., data = mydata3, hidden=10, threshold=0.1, startweights = winit, stepmax = 1e+9)
      
  
  plot(mydata3, cex=1, col="blue")
  points(mydata3[,1],predict(nn_revert,mydata3), col="red", cex=1) 
  legend("bottomleft", legend = c("Data", "Predictions"),
         pch = c(cex=1, cex=1),
         col=c("blue", "red"))  
  
    