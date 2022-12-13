# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 1 ------------------------------------------
# ----------------------------------------------------------------------------------------
# Reading the data
data <- read.csv("data/optidigits.csv", header = F)
data2 <- read.csv("data/optidigits.csv")

# Dividing in to training, validation and test
# n is the number of rows in optdigits.csv. Data[1] means we just count one column, so we only
# care about the nr of rows. 
library(dplyr)
n=dim(data)[1]
set.seed(12345) 
# id is 1911 randomly distributed integers between 1 and 3822.
id=sample(1:n, floor(n*0.5)) 
# train is the actual data on those rows 
train=data[id,] 
id1=setdiff(1:n, id)
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,] 

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 2 ------------------------------------------
# ----------------------------------------------------------------------------------------
# install.packages("kknn")
 library(kknn)

# tilde sign means that left is a function of right, where right = "." means all columns except
# the left side assigned columns. same as doing X0.26~X0.19 + X0.13....
# do View(kknn_train) to see the different outputs we get.
# The "C" values in kknn_train corresponds to the indexes(id:s) of the rows that are the 
# most similar to every each row. 
# Thats why we get 1911*k, because for each row we look at the k rows that are most similar.
# CL then shows the actual number written in those 8x8:s (the rightmost number, X0.26) for those 
# indexes. So: if for row one we get that 1, 1637 and 1845 are the most similar (k=3), 
# and their CL values are 0, we conclude that the fitted value is (0+0+0)/3 = 0
# Factoring 
train$V65 <- as.factor(train$V65)
test$V65 <- as.factor(test$V65)

kknn_train = kknn(V65~., train=train, test=train, kernel="rectangular", k = 30)
kknn_test = kknn(V65~., train=train, test=test, kernel="rectangular", k = 30)

# Predictions
train_predict = predict(kknn_train)
test_predict = predict(kknn_test)

# Confusion matrices
conf_train <- table(train_predict, train$V65)
conf_test <- table(test_predict, test$V65)

missclass <- function(X,Y) {
  n=length(X)
  return(1-sum(diag(table(X,Y)))/n)
}

missclass_train <- missclass(train_predict, train$V65)
missclass_test <- missclass(test_predict, test$V65)

#Comments on prediction quality - TRAIN: 1:s quite often confused with eights, and 9:s often confused 
# With fives. Missclass: 0.04239. TEST: Worse performance, missclass 0.04916. Quite often we think 
# sevens are fours. 

# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 3 ------------------------------------------
# ----------------------------------------------------------------------------------------  

# prob from kknn_train shows, for each of the 1911 rows, how many observations were each number. 
# For example, in row 2 we first identofy (in the C matrix) which 30 row indexes have the most
# similar row to row 2. CL then shows what the actual value was (högerledet). For row 2, 
# We then get that 25 of these rows where actually sevens => 0.833333 is the value for column 
# Seven in row 2. 

# all eights just gets all indexes from train data where hit was an eight.
all_eights <- train[which(train$V65 == "8"),]
# eight_probs first filters out the 1911x10 probability numerics from kknn_train, then sorts by 
# the rows (by index) that had eights. 
eight_probs <- kknn_train[["prob"]][which(train$V65 == "8"),9]
worst_probs <- sort(eight_probs, decreasing=F, index.return=T)$ix[1:3]
best_probs <- sort(eight_probs, decreasing=T, index.return=T)$ix[1:2]

actual_worst <- all_eights[worst_probs,]
actual_best <- all_eights[best_probs,]

worst.1 <- heatmap(t(matrix(as.numeric(actual_worst[1, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)
worst.2 <- heatmap(t(matrix(as.numeric(actual_worst[2, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)
worst.3 <- heatmap(t(matrix(as.numeric(actual_worst[3, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)

best.1 <- heatmap(t(matrix(as.numeric(actual_best[1, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)
best.2 <- heatmap(t(matrix(as.numeric(actual_best[2, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)


# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 4 ------------------------------------------
# ----------------------------------------------------------------------------------------

multi.missclass.train <- c()
multi.missclass.valid <- c()

for(x in 1:30) {
  #Train
  multi.train.kknn <- kknn(V65 ~., train = train, test = train, k = x, kernel = "rectangular")
  pred.train <- multi.train.kknn$fitted.values
  multi.missclass.train <- c(multi.missclass.train, missclass(pred.train, train$V65))
  #Valid
  multi.valid.kknn <- kknn(V65 ~., train = train, test = valid, k = x, kernel = "rectangular")
  pred.valid <- multi.valid.kknn$fitted.values
  multi.missclass.valid <- c(multi.missclass.valid, missclass(pred.valid, valid$V65))
}

best_k <- which.min(multi.missclass.valid)

test1.kknn <- kknn(V65 ~., train=train, test=test, k = best_k, kernel="rectangular")
pred.test1 <- test1.kknn$fitted.values
missclass_bestk <- missclass(pred.test1, test$V65)


#Create first line
plot(multi.missclass.train[1:30] * 100,
     type="b", frame = FALSE, col="green", ylim = c(0,10))

#Second line
lines(multi.missclass.valid[1:30] * 100, type="b", col="red")
lines(best_k, missclass_bestk*100, pch=1, type="b")


# ----------------------------------------------------------------------------------------
# -------------------------------------- Part 5 ------------------------------------------
# ----------------------------------------------------------------------------------------


multi_cross_entropy <- c()
loopsize <- nrow(valid)
for (i in 1:30) {
  multi_cross_kknn <- kknn(V65 ~., train = train, test = valid, k = i, kernel = "rectangular")
  cross_kknn_prob <- multi_cross_kknn[["prob"]]
  ce_value = 0
  for (current in 0:9) {
    for (j in 1:loopsize) {
      if (valid[j,65] == current) {
        ce_value = -1 * log(cross_kknn_prob[j, current + 1] + (1e-15)) + ce_value
      }
    }
  }
  multi_cross_entropy  <-  append(multi_cross_entropy, ce_value)
}
  # pred.cross <- multi_cross_kknn$fitted.values
  # char_cross <- as.character(pred.cross)
  # numeric_predicts <- as.numeric(char_cross)
  # 
  # multi_cross_entropy <- c(multi_cross_entropy, cross_entropy(valid$V65, numeric_predicts))

print(multi_cross_entropy)
plot(multi_cross_entropy, col="red")

which.min(multi_cross_entropy)
multi_cross_entropy[6]



