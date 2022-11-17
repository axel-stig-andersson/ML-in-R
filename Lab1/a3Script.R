# Assignment 3 
data <- read.csv("data/pima-diabetes.csv", head=F)
attach(data)

library(ggplot2)
diabetes.groups <- as.factor(ifelse(V9, "Diabetes", "Healthy"))
scatter.plot <- plot(x=V8, y=V2, main = "Plot Age vs Glucose conc", xlab = "Age", ylab = "PG concentration", 
                     pch=as.numeric(diabetes.groups), col=diabetes.groups)
legend(70,50, legend = c("Diabetes", "Healthy"), col = diabetes.groups, pch = diabetes.groups)
#gg.scatter <- ggplot(data=data,) 

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 2 ------------------------------------------
# ----------------------------------------------------------------------------------------

data.log.reg <- glm(V9~V8+V2, family = "binomial") 
pred.log.req <- predict(data.log.reg, data, type = "response")
pred.log.req <- ifelse(pred.log.req > 0.5, 1, 0) 
confusion.matrix <- table(pred.log.req, V9)
missclass <- 1 - sum(diag(confusion.matrix))/length(pred.log.req)

pred.diabetes.groups <- as.factor(ifelse(pred.log.req, "Predicted Diabetes", "Predicted Healthy"))
pred.plot <- plot(x=V8, y=V2, main = "Prediction plot", xlab = "Age", ylab = "PG concentration", 
                  pch=as.numeric(pred.diabetes.groups), col=pred.diabetes.groups)
legend("bottomright", legend = c("Pred. diabetes", "Pred. healthy"), col = pred.diabetes.groups, pch = pred.diabetes.groups)


# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 3 ------------------------------------------
# ----------------------------------------------------------------------------------------

slope <- coef(data.log.reg)[2]/(-coef(data.log.reg)[3])
intercept <- coef(data.log.reg)[1]/(-coef(data.log.reg)[3])
abline(intercept, slope) 

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 4 ------------------------------------------
# ----------------------------------------------------------------------------------------
# For r values 0.2 and 0.8. Higher value on r means that we require higher indications of healthy

pred.log.req.2 <- predict(data.log.reg, data, type = "response")
pred.log.req.2 <- ifelse(pred.log.req.2 > 0.2, 1, 0) 
confusion.matrix.2 <- table(pred.log.req.2, V9)
missclass.2 <- 1 - sum(diag(confusion.matrix.2))/length(pred.log.req.2)
pred.diabetes.groups.2 <- as.factor(ifelse(pred.log.req.2, "Predicted Diabetes", "Predicted Healthy"))
pred.plot.2 <- plot(x=V8, y=V2, main = "Prediction plot 0.2", xlab = "Age", ylab = "PG concentration", 
                    pch=as.numeric(pred.diabetes.groups.2), col=pred.diabetes.groups.2)
legend("bottomright", legend = c("Pred. diabetes", "Pred. healthy"), col = pred.diabetes.groups.2, pch = pred.diabetes.groups.2)

################################### FOR 0.8 #################################
pred.log.req.8 <- predict(data.log.reg, data, type = "response")
pred.log.req.8 <- ifelse(pred.log.req.8 > 0.8, 1, 0) 
confusion.matrix.8 <- table(pred.log.req.8, V9)
missclass.8 <- 1 - sum(diag(confusion.matrix.8))/length(pred.log.req.8)
pred.diabetes.groups.8 <- as.factor(ifelse(pred.log.req.8, "Predicted Diabetes", "Predicted Healthy"))
pred.plot.8 <- plot(x=V8, y=V2, main = "Prediction plot 0.8", xlab = "Age", ylab = "PG concentration", 
                    pch=as.numeric(pred.diabetes.groups.8), col=pred.diabetes.groups.8)
legend("bottomright", legend = c("Pred. diabetes", "Pred. healthy"), col = c(2,1), pch = c(2,1))

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 5 ------------------------------------------
# ----------------------------------------------------------------------------------------
# Creating the new columns
data$Z1 = V2^4
data$Z2 = (V2^3)*V8
data$Z3 = (V2^2)*(V8^2)
data$Z4 = V2*(V8^3)
data$Z5 = V8^4

# Performing the scatterplot
data.reg.new.cols <- glm(V9~V2+V8+Z1+Z2+Z3+Z4+Z5, family = "binomial")

pred.Z <- predict(data.reg.new.cols, data, type = "response")
pred.Z.Bin <- ifelse(pred.Z > 0.5, 1, 0) 

confusion.matrix.Z <- table(pred.Z.Bin, V9)
missclass.Z <- 1 - sum(diag(confusion.matrix.Z))/length(pred.Z.Bin)

pred.diabetes.Z <- as.factor(ifelse(pred.Z.Bin, "Predicted Diabetes", "Predicted Healthy"))
pred.plot.Z <- plot(x=V8, y=V2, main = "Prediction plot", xlab = "Age", ylab = "PG concentration", 
                  pch=as.numeric(pred.diabetes.Z), col=pred.diabetes.Z)
legend("bottomright", legend = c("Pred. diabetes", "Pred. healthy"), col = pred.diabetes.Z, pch = pred.diabetes.Z)


