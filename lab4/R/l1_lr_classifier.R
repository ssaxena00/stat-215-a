# L1_LR Regression Classifier #

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

# ROC Plot #
tprfpr_full$fold = 1
tprfpr_sub$fold = 2
tprfpr <- rbind(tprfpr_full, tprfpr_sub)
tprfpr$fold <- as.factor(tprfpr$fold)
saveRDS(tprfpr, "data/L1LR_tprfpr.rds")
p = ggplot(tprfpr, aes(x=fpr, y=tpr, group = fold, color = fold))
p = p + geom_abline(slope=1, intercept=0, size=1.5)
p = p + geom_line(aes(color = fold), size = 1.5)
p = p + scale_color_manual(name = "Model", labels = c("NDAI, SD, CORR", "All"), values=c("brown", "dodgerblue"))
p = p + labs(title="ROC Curve", x="FPR", y="TPR")
p = p + theme_bw(18)
p = p + theme(plot.title = element_text(size = 20, face="bold", hjust=0.5))
p = p + theme(plot.subtitle = element_text(size = 14, face="bold", hjust=0.5))
p

df_cv_full <- data.frame(lambda = myglmnet_full$lambda, cv = colMeans(cv_mat_full), fold = 1)
df_cv_sub <- data.frame(lambda = myglmnet_sub$lambda, cv = colMeans(cv_mat_sub), fold = 2)
df_cv <- rbind(df_cv_full, df_cv_sub)
df_cv$fold <- as.factor(df_cv$fold)
saveRDS(df_cv, "data/L1LR_df_cv.rds")
p = ggplot(df_cv, aes(x=lambda, y=cv, group = fold, color = fold))
p = p + geom_point(aes(color = fold), size = 1.5)
p = p + labs(title="CV Accuracy by Lambda", x="Lambda", y="CV Accuracy") 
p = p + scale_color_manual(name = "Model", labels = c("All", "NDAI, SD, CORR"), values=c("brown", "dodgerblue"))
p = p + scale_x_continuous(trans = "log10")
p = p + theme_bw(18)
p = p + theme(plot.title = element_text(size = 20, face="bold", hjust=0.5))
p = p + theme(plot.subtitle = element_text(size = 14, face="bold", hjust=0.5))
p



