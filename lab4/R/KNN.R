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
    dplyr::select(c(features, "label")) %>%
    filter(label != 0) %>% 
    dplyr::select(-label) %>%
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

# plotting accuracy of KNN
knnAccuracyPlot <- function (data, plot) {
  p1 <- ggplot(data, aes(x = k, y = accuracy)) +
    geom_point() + geom_line(size = 1.2) + 
    geom_vline(xintercept = c(31), linetype = "dotted") +
    ylab("Accuracy") +
    geom_label(data = . %>% filter(accuracy == max(accuracy)),
               aes(label = paste("31,", sprintf('%0.2f', accuracy))),
               hjust = -0.2, size = 3) + 
    theme(text = element_text(size = 12), axis.title = element_text(size = 18))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# return plot of ROC for multiple trained objects
rocPlot <- function (roc_df, plot) {
  # roc_df: df of roc with type column for each trained object
  p1 <- ggplot(roc_df) +
    geom_line(size = 1.4, aes(x = false_pos, y = true_pos, color = type)) +
    geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "red") +
    xlab("False Positive Rate") + ylab("True Positive Rate") +
    theme(axis.title = element_text(size = 20), axis.text=element_text(size=18)) +  
    guides(color = guide_legend(title = "KNN Model")) +
    scale_colour_manual(values = c("deepskyblue3", "seagreen3")) + 
    theme_bw()
  if (plot == TRUE) {
    print(p1)
  }
  return(p1)
}

# plotting convex hull of classifications
boundaryPlot <- function (test, prediction, feature1, feature2, plot) {
  # use convex hull to determine boundary points of clusters
  boundary_df <- data.frame(x = test[, feature1], 
                            y = test[, feature2], 
                            Prediction = prediction)
  findHull <- function(df) {
    df[chull(df$x, df$y),]
  }
  boundary <- plyr::ddply(boundary_df, .variables = "Prediction", .fun = findHull)
  p1 <- ggplot() + 
    geom_point(data = boundary_df, size = 2,
               aes(x = x, y = y, color = Prediction, fill = Prediction)) +
    geom_polygon(data = boundary, alpha = 0.4, aes(x = x, y = y, fill = Prediction, color = Prediction)) +
    xlab(feature1) + ylab(feature2) + 
    scale_fill_manual(name = "Prediction",
                      label = c("Not Cloud", "Cloud"),
                      values = c("deepskyblue", "palegreen2")) + 
    scale_color_manual(name = "Prediction",
                       label = c("Not Cloud", "Cloud"),
                       values = c("blueviolet", "springgreen4")) +
    theme_bw() +
    theme(text = element_text(size = 12), axis.title = element_text(size = 12))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

