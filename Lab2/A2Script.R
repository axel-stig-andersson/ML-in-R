# Assignment 2

#---------------------------------------------------------------------------------
#------------------------------------Task 1---------------------------------------
#---------------------------------------------------------------------------------
library(dplyr)
data <- read.csv2("Data/bank-full.csv", stringsAsFactors = TRUE)
data <- subset(data, select = -c(duration))

n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.4)) 
train=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.3)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 
View()
#---------------------------------------------------------------------------------
#------------------------------------Task 2---------------------------------------
#---------------------------------------------------------------------------------
library(tree)
# Tree control tar in parametrar som ställer krav på hur det får se ut. 
# Man skriver det som så att trädet tar in en parameter control som är lika med 
# tree.control(x,y,z), där x = mincut (minimum antal observationer i varje child node
# vid delning), y = minsize (minsta tillåtna nodstorlek. default 10), z = mindev (fattar 
# inte riktigt än)
tree_default <- tree(as.factor(y)~., data = train)
tree_7000 <- tree(as.factor(y)~., data = train, control = tree.control(nrow(train), 
                                                                       minsize = 7000))
tree_deviance <- tree(as.factor(y)~., data = train, control = tree.control(nrow(train),
                                                                           mindev = 0.0005))
# Predictions on train
library(dplyr)
train_input <- train%>%select(-y)
pred_train_1 <- predict(tree_default, newdata = train_input)
pred_train_2 <- predict(tree_7000, newdata = train_input)
pred_train_3 <- predict(tree_deviance, newdata = train_input)

# Predictions on validation
valid_input <- valid%>%select(-y)
pred_valid_1 <- predict(tree_default, newdata = valid_input)
pred_valid_2 <- predict(tree_7000, newdata = valid_input)
pred_valid_3 <- predict(tree_deviance, newdata = valid_input)

# Classified train predictions
class_train_1 <- pred_train_1[,1]
class_train_1 <- as.matrix(ifelse(class_train_1 > 0.5, "no", "yes"))

class_train_2 <- pred_train_2[,1]
class_train_2 <- as.matrix(ifelse(class_train_2 > 0.5, "no", "yes"))

class_train_3 <- pred_train_3[,1]
class_train_3 <- as.matrix(ifelse(class_train_3 > 0.5, "no", "yes"))

# Classified validation prediction
class_valid_1 <- pred_valid_1[,1]
class_valid_1 <- as.matrix(ifelse(class_valid_1 > 0.5, "no", "yes"))

class_valid_2 <- pred_valid_2[,1]
class_valid_2 <- as.matrix(ifelse(class_valid_2 > 0.5, "no", "yes"))

class_valid_3 <- pred_valid_3[,1]
class_valid_3 <- as.matrix(ifelse(class_valid_3 > 0.5, "no", "yes"))

missclass1 = function(actual, pred_class) {
  sum <- 0
  size <- nrow(pred_class)
  for (i in 1:size) {
    if (actual[i] != pred_class[i]) {
      sum = sum + 1
    }
  }
  return(sum/size)
}

# Missclassifications train
mc_train_1 <- missclass1(train$y, class_train_1)
mc_train_2 <- missclass1(train$y, class_train_2)
mc_train_3 <- missclass1(train$y, class_train_3)

# Missclassifications valid
mc_valid_1 <- missclass1(valid$y, class_valid_1)
mc_valid_2 <- missclass1(valid$y, class_valid_2)
mc_valid_3 <- missclass1(valid$y, class_valid_3)


#---------------------------------------------------------------------------------
#------------------------------------Task 3---------------------------------------
#---------------------------------------------------------------------------------
# package mlmetrics

trainScore=rep(0,50)
validScore=rep(0,50)
for(i in 2:50) {
  prunedTree=prune.tree(tree_deviance, best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)
  validScore[i]=deviance(pred)
  }
plot(2:50, trainScore[2:50], type="b", col="red", 
     ylim=c(7500,12500)) 
points(2:50, validScore[2:50], type="b", col="blue")   

# Mnimizing to choose optimal number of terminal nodes
best_val <- which.min(validScore[2:50]) + 1 

# =================================================================================
# ==============================Hugges plot, borde kolla ==========================
# =================================================================================
# score_train=rep(0,50)
# score_valid=rep(0,50)
# for(i in 2:50) {
#   prunedTree <- prune.tree(tree_c,best=i) 
#   pred <- predict(prunedTree, newdata=valid,type="tree") 
#   score_train[i] <- deviance(prunedTree)
#   score_valid[i] <- deviance(pred)
# }
# plot(2:50, score_train[2:50], type="b", col="red",pch=20, ylim=c(8000,12000), ylab = 'Deviance', xlab = "Leaves")
# points(2:50, score_valid[2:50], type="b", col="blue", pch=20)
# legend("topleft",
#        legend = c("Deviance", "Predicted deviance"),
#        col = c("darkgreen", "darkred"), lty=1:2, cex = 0.8, bg="white"
# )
# 

#---------------------------------------------------------------------------------
#------------------------------------Task 4---------------------------------------
#---------------------------------------------------------------------------------
library(MLmetrics)
library(rpart)
# Data manipulation to obtain matrix for the table later

optimal_tree <- prune.tree(tree_deviance, best = best_val)
input_test <- test%>%select(-y)
pred_optimal <- predict(optimal_tree, newdata = input_test)
class_optimal <- pred_optimal[,1]
class_optimal <- as.matrix(ifelse(class_optimal > 0.5, "no", "yes"))
 # Using the class_optimal matrix to obtain the confusion matrix.
# Testa lek med både caret och MLmetrics grejer
confusion_opt <- table(class_optimal, test$y)

# Overall accuracy
accuracy_opt <- Accuracy(class_optimal, test$y)
# F1 Score 
F1_opt <- F1_Score(test$y, class_optimal, positive = NULL)

#---------------------------------------------------------------------------------
#------------------------------------Task 5---------------------------------------
#---------------------------------------------------------------------------------
loss_matrix <- matrix(data = c(0,1,5,0), nrow=2, byrow = TRUE)
lmtrx_tree <- rpart(y~., data = test, method = "class", 
                         parms = list(loss=loss_matrix))
pred_lmtrx <- predict(lmtrx_tree, newdata = input_test, type = "class")
# class_lmtrx <- pred_lmtrx[,1]
# class_lmtrx <- as.matrix(ifelse(class_lmtrx > 0.5, "no", "yes"))
confusion_lmtrx <- table(pred_lmtrx, test$y)

#---------------------------------------------------------------------------------
#------------------------------------Task 6---------------------------------------
#---------------------------------------------------------------------------------
optimal_tree <- prune.tree(tree_deviance, best = best_val)
pred_optimal <- predict(optimal_tree, newdata = input_test)

pi_values <- seq(0.05, 0.95, 0.05)
# TPR = TP/P, FPR = FP/N # TRP = TP / (TP + FN)
setsize <- length(pi_values)
TPR <- c()
FPR <- c()

for (i in 1:setsize){
  pred_optimal_desicion <- ifelse(pred_optimal[,2]>pi_values[i],"yes","no")
  pred_optimal_matrix = table(pred_optimal_desicion, test$y)
  #TRP = TP / TP+FN, FPR =  FP / FP + TN, as said the n
  current_tp <- pred_optimal_matrix[4]
  current_fp <- pred_optimal_matrix[2]
  all_pos <- pred_optimal_matrix[3] + pred_optimal_matrix[4]
  all_neg <- pred_optimal_matrix[1] + pred_optimal_matrix[2]
  TPR <- c(TPR, current_tp/all_pos)
  FPR <- c(FPR, current_fp/all_neg)
}
plot(FPR, TPR)


