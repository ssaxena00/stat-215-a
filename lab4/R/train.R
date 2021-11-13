# cross validation for KNN - parameter tuning k
knnCrossVal <- function (data, k_seq, fold) {
  # data: data frame object with labels as factors, *features scaled*
  # k_seq: k values to test
  # return: list of accuracy and kappa for each k
  trControl <- trainControl(method = "cv", number = fold)
  fit <- train(label~.,
               method = "knn",
               tuneGrid = expand.grid(k = k_seq),
               trControl = trControl,
               metric = "Accuracy",
               data = data)
  return (fit$results[, c("k", "Accuracy", "Kappa")])
}

# create dataframe compatible for training KNN
trainData <- function (image, features) {
  data <- image %>% 
    select(c(features, "label")) %>%
    filter(label != 0) %>% 
    select(-label) %>%
    scale() %>% as.data.frame()
  data$label <- image %>% 
    filter(label != 0) %>%
    mutate(label = as.factor(label)) %>% pull(label)
  return (data)
}

# calculate AUC and accuracy of KNN on test set
knnEval <- function (train, test, k) {
  pred <- knn(train = train %>% select(-label),
              test = test %>% select(-label),
              cl = unlist(train$label), k = k, prob = TRUE)
  prob <- attr(pred, "prob")
  prob <- (2 * ifelse(pred == "-1", 1 - prob, prob)) - 1
  pred_knn <- prediction(prob, unlist(test$label))
  
  # calculate AUC
  auc.tmp <- performance(pred_knn, "auc")
  auc <- as.numeric(auc.tmp@y.values)
  print("AUC:")
  print(auc)
  
  # calculate accuracy
  cm = as.matrix(table(Actual = unlist(test$label), Predicted = pred))
  accuracy <- sum(diag(cm))/length(unlist(test$label))
  print("Accuracy:")
  print(accuracy)
  
  return (c(auc, accuracy))
}


# looking at AUC for k_seq, train and test are scaled dataframes - probably delete
optimalK <- function (k_seq, train, test, train_labels, test_labels) {
  aucs <- c()
  for (k in k_seq) {
    a <- Sys.time()
    pred <- knn(train = train, test = test, cl = train_labels, k = k, prob = TRUE)
    prob <- attr(pred, "prob")
    prob <- (2 * ifelse(pred == "-1", 1 - prob, prob)) - 1
    pred_knn <- prediction(prob, test_labels)
    auc.tmp <- performance(pred_knn, "auc")
    auc <- as.numeric(auc.tmp@y.values)
    print(k)
    print(auc)
    aucs <- c(aucs, auc)
    b <- Sys.time() - a
    print(b)
  }
  return (aucs)
}

# return geom_line object of ROC for KNN
rocDF <- function (pred, test) {
  prob <- attr(pred, "prob")
  prob <- (2 * ifelse(pred == "-1", 1 - prob, prob)) - 1
  pred_knn <- prediction(prob, unlist(test$label))
  pred_knn <- performance(pred_knn, "tpr", "fpr")
  pred_knn <- data.frame(true_pos = as.vector(pred_knn@y.values[[1]]), false_pos = as.vector(pred_knn@x.values[[1]]))
  return (pred_knn)
}

