library(ggplot2)
library(glmnet)
library(ROCR)

# path
path <- "data/"

# load unzipped data
image1 <- read.table("data/image1.txt")
image2 <- read.table("data/image2.txt")
image3 <- read.table("data/image3.txt")

# changing column names
columns <- c("y_coord", "x_coord",
             "label", "NDAI", "SD", "CORR",
             "DF", "CF", "BF", "AF", "AN")
colnames(image1) = colnames(image2) = colnames(image3) = columns

train_data <- rbind(image1[4:ncol(image1)], image2[4:ncol(image2)])
test_data <- image3[, 4:ncol(image3)]
train_outcome <- c(image1$label, image2$label)
test_outcome <- image3$label

train_data <- train_data[-which(train_outcome == 0), ]
train_data <- as.matrix(train_data)
test_data <- test_data[-which(test_outcome == 0), ]
test_data <- as.matrix(test_data)
train_outcome <- train_outcome[-which(train_outcome == 0)]
test_outcome <- test_outcome[-which(test_outcome == 0)]

# Logisitic LASSO
# outputs predicted probabilities from leave-one-out sparse logistics regression 
# and counts of significant coefficents from each iteration of running leave-one-out
lasso_results <- function(data, outcome, test_data, alpha_type){
  # stores coefficients
  coeff_mat = matrix(NA, nrow = ncol(data)+1, ncol=length(unique(row.names(data))))
  rownames(coeff_mat) = c("Intercept", colnames(data))
  # glmnet object
  myglmnet = cv.glmnet(data, outcome, family = "binomial", type.measure="deviance", alpha=alpha_type, standardize=TRUE, nfolds=10)
  # store coefficients from model
  sigmeta <- coef(myglmnet, s = "lambda.min")
  coeff_mat = matrix(sigmeta)
  # predicted probabilities for test points
  test_glmnet <- predict(myglmnet, test_data, type="response", s="lambda.1se")
  return(list(test_glmnet, myglmnet, coeff_mat))
}

results_l <- lasso_results(train_data, train_outcome, test_data, alpha_type = 1)
pred_l <- prediction(results_l[[1]], test_outcome)
auc_l <- performance(pred_l, measure="auc")@y.values[[1]]
perf_l <- performance(pred_l, measure="tpr", x.measure="fpr")
tprfpr_l <- data.frame(fpr=perf_l@x.values[[1]], fpr=perf_l@y.values[[1]])

results_r <- lasso_results(train_data, train_outcome, test_data, alpha_type = 0)
pred_r <- prediction(results_r[[1]], test_outcome)
auc_r <- performance(pred_r, measure="auc")@y.values[[1]]
perf_r <- performance(pred_r, measure="tpr", x.measure="fpr")
tprfpr_r <- data.frame(fpr=perf_r@x.values[[1]], tpr=perf_r@y.values[[1]])

# ROC Plot #
p = ggplot(tprfpr_r, aes(x=fpr, y=tpr))
p = p + geom_abline(slope=1, intercept=0, size=1.5)
p = p + geom_line(size = 1.5, colour = "red")
p = p + labs(title="ROC Curve", x="FPR", y="TPR")
p = p + theme_bw(18)
p = p + theme(plot.title = element_text(size = 20, face="bold", hjust=0.5))
p = p + theme(plot.subtitle = element_text(size = 14, face="bold", hjust=0.5))
p

