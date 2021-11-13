# Ridge Regression Classifier #

library(ggplot2)
library(glmnet)
library(ROCR)

load("data/SplitEachImage")
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
myglmnet <- glmnet(train_r[, 4:ncol(train_r)], train_r$label, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
cv_mat <- matrix(NA, nrow = 10, ncol = length(myglmnet$lambda))
# Run CV
for (i in 1:10) {
  print(i)
  cv_train <- train_r[tr_ind[[i]], ]
  cv_val <- train_r[val_ind[[i]], ]
  glmnet_fold[[i]] <- glmnet(cv_train[, 4:ncol(cv_train)], cv_train$label, lambda = myglmnet$lambda, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)

  for(j in 1:length(glmnet_fold[[i]]$lambda)){
    predprobs = predict(glmnet_fold[[i]], as.matrix(cv_val[, 4:ncol(cv_val)]), type="response", s=glmnet_fold[[i]]$lambda[j])
    pred <- prediction(predprobs, cv_val$label)
    auc_val <- performance(pred, measure="auc")@y.values[[1]]
    
    acc <- mean(sign((predprobs > 0.5) - 0.5) == cv_val$label)
    
    cv_mat[i, j] <- acc
  }
}
myglmnet_final_full <- glmnet(train_r[, 4:ncol(train_r)], train_r$label, lambda = myglmnet$lambda[which.max(colMeans(cv_mat))], family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE)
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

# ROC Plot #
p = ggplot(tprfpr_full, aes(x=fpr, y=tpr))
p = p + geom_abline(slope=1, intercept=0, size=1.5)
p = p + geom_line(size = 1.5, colour = "red")
p = p + labs(title="ROC Curve", x="FPR", y="TPR")
p = p + theme_bw(18)
p = p + theme(plot.title = element_text(size = 20, face="bold", hjust=0.5))
p = p + theme(plot.subtitle = element_text(size = 14, face="bold", hjust=0.5))
p

