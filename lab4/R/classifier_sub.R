# Ridge Regression Classifier #

library(glmnet)
library(ROCR)

load("../data/SplitEachImage")
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
myglmnet <- glmnet(train_r[, 4:6], train_r$label, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
cv_mat <- matrix(NA, nrow = 10, ncol = length(myglmnet$lambda))
# Run CV
for (i in 1:10) {
  print(i)
  cv_train <- train_r[tr_ind[[i]], ]
  cv_val <- train_r[val_ind[[i]], ]
  glmnet_fold[[i]] <- glmnet(cv_train[, 4:6], cv_train$label, lambda = myglmnet$lambda, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
  
  for(j in 1:length(glmnet_fold[[i]]$lambda)){
    predprobs = predict(glmnet_fold[[i]], as.matrix(cv_val[, 4:6]), type="response", s=glmnet_fold[[i]]$lambda[j])
    pred <- prediction(predprobs, cv_val$label)
    auc_val <- performance(pred, measure="auc")@y.values[[1]]
    
    acc <- mean(sign((predprobs > 0.5) - 0.5) == cv_val$label)
    
    cv_mat[i, j] <- acc
  }
}
myglmnet_final <- glmnet(train_r[, 4:6], train_r$label, lambda = myglmnet$lambda[which.max(colMeans(cv_mat))], family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
predprobs = predict(myglmnet_final, as.matrix(test_r[, 4:6]), type="response", s=myglmnet_final$lambda)
pred <- prediction(predprobs, test_r$label)
auc_val <- performance(pred, measure="auc")@y.values[[1]]
acc <- mean(sign((predprobs > 0.5) - 0.5) == test_r$label)
acc_val <- max(performance(pred, measure="acc")@y.values[[1]])
auc_val
acc_val
acc

