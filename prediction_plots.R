options(stringsAsFactors = FALSE)
set.seed(614)
library(glmnet)
library(randomForest)
library(ROCR)
library(ggplot2)
# Dimensions of data
rows_test <- 1000
cols_test <- 100

create_matching_data <- function(rows = 1000, columns = 5, nonlinear = TRUE){
  simulated_data <- as.data.frame(matrix(rnorm(rows*columns), rows, columns))
  if(nonlinear == TRUE){
    simulated_data <- cbind(simulated_data, simulated_data^2, simulated_data^3)
  }
  as.matrix(simulated_data)
}
sample_data1 <- create_matching_data(
  rows = rows_test, columns = cols_test, nonlinear = FALSE)
sample_data1 <- cbind(1, sample_data1)
colnames(sample_data1) <- paste0("A", seq(1:ncol(sample_data1)))

covariates <- as.matrix(rnorm(ncol(sample_data1), 0, 1))
covariates[1,] <- -7
lp_out <- as.matrix(sample_data1[,1:round(.35*cols_test)]) %*% as.matrix(covariates[1:round(.35*cols_test),]) + rnorm(rows_test, 0, 1)
pr_out <- plogis(lp_out)
sample_data1 <- as.data.frame(sample_data1)
sample_data1$outcome <-rbinom(rows_test, 1, pr_out)
sample_data <- sample_data1
sample_data$outcome <- sample_data1$outcome
sample_train <- data.frame(sample_data[1:nrow(sample_data)*.8,])
sample_test <- sample_data[((nrow(sample_data)*.8)+1):nrow(sample_data),]

model_formula <- paste0(names(sample_train[,1:ncol(sample_data)-1]), collapse = "+")
model_formula <- formula(paste0("outcome ~ ", model_formula))
logit1 <- glm(model_formula, family = binomial(link = "logit"), data = sample_train)
elastic_net1 <- glmnet(
  x = as.matrix(sample_train[,1:ncol(sample_train)-1]), 
  y = sample_train$outcome, family = c("binomial"))
rf1 <- randomForest(x = as.matrix(sample_train[,1:ncol(sample_train)-1]), 
                    y = factor(sample_train$outcome), ntree = 1000)

logit1_predict <- predict(logit1, newdata = sample_test, type = "response")
elastic_net1_predict <- predict(
  elastic_net1, newx = as.matrix(sample_test[,1:ncol(sample_test)-1]), 
  s = mean(elastic_net1$lambda), type = "response")
rf1_predict <- predict(
  rf1, newdata = as.matrix(sample_test[,1:ncol(sample_data)-1]), type = "prob")

preds_log <- prediction(logit1_predict, sample_test[,ncol(sample_test)])
perfs_log <- performance(preds_log, "tpr","fpr")
roc_data_log <- data.frame(fpr = unlist(perfs_log@x.values), 
                           tpr=unlist(perfs_log@y.values))
roc_data_log$Model <- "Logistic Regression"
preds_net <- prediction(elastic_net1_predict, sample_test[,ncol(sample_test)])
perfs_net <- performance(preds_net, "tpr","fpr")
roc_data_net <- data.frame(fpr = unlist(perfs_net@x.values), 
                           tpr=unlist(perfs_net@y.values))
roc_data_net$Model <- "Elastic Net"
preds_rf <- prediction(rf1_predict[,2], sample_test[,ncol(sample_test)])
perfs_rf <- performance(preds_rf, "tpr","fpr")
roc_data_rf <- data.frame(fpr = unlist(perfs_rf@x.values), 
                          tpr=unlist(perfs_rf@y.values))
roc_data_rf$Model <- "Random Forest"
roc_data <- rbind(roc_data_log, roc_data_net, roc_data_rf)
ggplot(data = roc_data, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = Model)) + theme_minimal() + 
  geom_abline(data = data.frame(x = seq(0,1,.01), y = seq(0,1,.01)), 
              intercept = 0, lty = 2) + 
  labs(title = "ROC Plot for Three Models", x = "False Positive Rate", 
       y = "True Positive Rate")

# pr curve
perfs_log_pr <- performance(preds_log, "prec","rec")
roc_data_log_pr <- data.frame(rec = unlist(perfs_log_pr@x.values), 
                              prec=unlist(perfs_log_pr@y.values))
roc_data_log_pr$Model <- "Logistic Regression"
perfs_net_pr <- performance(preds_net, "prec","rec")
roc_data_net_pr <- data.frame(rec = unlist(perfs_net_pr@x.values), 
                              prec=unlist(perfs_net_pr@y.values))
roc_data_net_pr$Model <- "Elastic Net"

preds_rf_pr <- prediction(rf1_predict[,2], sample_test[,ncol(sample_test)])
perfs_rf_pr <- performance(preds_rf_pr,  "prec","rec")
pr_data_rf <- data.frame(rec = unlist(perfs_rf_pr@x.values), 
                         prec=unlist(perfs_rf_pr@y.values))
pr_data_rf$Model <- "Random Forest"

prc_data <- rbind(roc_data_log_pr, roc_data_net_pr, pr_data_rf)
ggplot(data = prc_data, aes(x = rec, y = prec)) + 
  geom_line(aes(color = Model)) + theme_minimal() + 
  geom_abline(data = data.frame(x = seq(0,1,.01), y = seq(0.1,.01)), 
              intercept = 1, slope = -1, lty = 2)+ 
  labs(title = "Precision-Recall Curve Plot for Three Models", 
       x = "Recall", 
       y = "Precision")