# L1-LR Regression Classifier #

library(ggplot2)
library(glmnet)
library(ROCR)

# load unzipped data
image1 <- read.table("data/image1.txt")
image2 <- read.table("data/image2.txt")
image3 <- read.table("data/image3.txt")
# changing column names
columns <- c("y_coord", "x_coord",
             "label", "NDAI", "SD", "CORR",
             "DF", "CF", "BF", "AF", "AN")
colnames(image1) = colnames(image2) = colnames(image3) = columns
train_r <- rbind(image1, image2)
train_r <- train_r[-which(train_r$label == 0), ]
test_r <- image3[-which(image3$label == 0), ]
ntr = nrow(train_r)

set.seed(12345)
cv_rng <- sample(ntr, ntr)

# Index for validation (left out) CV fold
val_ind <- split(cv_rng, rep_len(1:10, ntr))

# Index for train CV folds
tr_ind <- list()
for (i in 1:10) {
  tr_ind[[i]] <- setdiff(1:ntr, val_ind[[i]])
}

# Run cross validation
alpha_type = 0 # Ridge
glmnet_fold = list() # glmnet object from each fold
# Run glmnet to get a lambda sequence to use on each fold when doing CV
myglmnet_full <- glmnet(train_r[, 4:ncol(train_r)], train_r$label, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
cv_mat_full <- matrix(NA, nrow = 10, ncol = length(myglmnet_full$lambda))
# Run CV
for (i in 1:10) {
  print(i)
  cv_train <- train_r[tr_ind[[i]], ]
  cv_val <- train_r[val_ind[[i]], ]
  glmnet_fold[[i]] <- glmnet(cv_train[, 4:ncol(cv_train)], cv_train$label, lambda = myglmnet_full$lambda, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
  
  for(j in 1:length(glmnet_fold[[i]]$lambda)){
    predprobs = predict(glmnet_fold[[i]], as.matrix(cv_val[, 4:ncol(cv_val)]), type="response", s=glmnet_fold[[i]]$lambda[j])
    pred <- prediction(predprobs, cv_val$label)
    auc_val <- performance(pred, measure="auc")@y.values[[1]]
    
    acc <- mean(sign((predprobs > 0.5) - 0.5) == cv_val$label)
    
    cv_mat_full[i, j] <- acc
  }
}
myglmnet_final_full <- glmnet(train_r[, 4:ncol(train_r)], train_r$label, lambda = myglmnet_full$lambda[which.max(colMeans(cv_mat_full))], family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
predprobs_full = predict(myglmnet_final_full, as.matrix(test_r[, 4:ncol(test_r)]), type="response", s=myglmnet_final_full$lambda)
pred_full <- prediction(predprobs_full, test_r$label)
auc_val_full <- performance(pred_full, measure="auc")@y.values[[1]]
perf_full <- performance(pred_full, measure="tpr", x.measure="fpr")
tprfpr_full <- data.frame(fpr=perf_full@x.values[[1]], tpr=perf_full@y.values[[1]])
acc_full <- mean(sign((predprobs_full > 0.5) - 0.5) == test_r$label)
acc_val_full <- max(performance(pred_full, measure="acc")@y.values[[1]])
auc_val_full
acc_val_full
acc_full

# Run cross validation
alpha_type = 0 # Ridge
glmnet_fold = list() # glmnet object from each fold
# Run glmnet to get a lambda sequence to use on each fold when doing CV
myglmnet_sub <- glmnet(train_r[, 4:6], train_r$label, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
cv_mat_sub <- matrix(NA, nrow = 10, ncol = length(myglmnet_sub$lambda))
# Run CV
for (i in 1:10) {
  print(i)
  cv_train <- train_r[tr_ind[[i]], ]
  cv_val <- train_r[val_ind[[i]], ]
  glmnet_fold[[i]] <- glmnet(cv_train[, 4:6], cv_train$label, lambda = myglmnet_sub$lambda, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
  
  for(j in 1:length(glmnet_fold[[i]]$lambda)){
    predprobs = predict(glmnet_fold[[i]], as.matrix(cv_val[, 4:6]), type="response", s=glmnet_fold[[i]]$lambda[j])
    pred <- prediction(predprobs, cv_val$label)
    auc_val <- performance(pred, measure="auc")@y.values[[1]]
    
    acc <- mean(sign((predprobs > 0.5) - 0.5) == cv_val$label)
    
    cv_mat_sub[i, j] <- acc
  }
}
myglmnet_final_sub <- glmnet(train_r[, 4:6], train_r$label, lambda = myglmnet_sub$lambda[which.max(colMeans(cv_mat_sub))], family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
predprobs_sub = predict(myglmnet_final_sub, as.matrix(test_r[, 4:6]), type="response", s=myglmnet_final_sub$lambda)
pred_sub <- prediction(predprobs_sub, test_r$label)
auc_val_sub <- performance(pred_sub, measure="auc")@y.values[[1]]
perf_sub <- performance(pred_sub, measure="tpr", x.measure="fpr")
tprfpr_sub <- data.frame(fpr=perf_sub@x.values[[1]], tpr=perf_sub@y.values[[1]])
acc_sub <- mean(sign((predprobs_sub > 0.5) - 0.5) == test_r$label)
acc_val_sub <- max(performance(pred_sub, measure="acc")@y.values[[1]])
auc_val_sub
acc_val_sub
acc_sub


