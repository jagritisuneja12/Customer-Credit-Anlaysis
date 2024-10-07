# Load required libraries
library(tidyverse)
library(dplyr)
library(factoextra)
library(ggplot2)
library(GGally)
library(corrplot)
library(psych)
library(glmnet)
library(fastDummies)
library(caret)
library(ca)
library(vcd)
library(FactoMineR)
library(GPArotation)

# Read the dataset
clients <- read.csv("/Users/jagritisuneja/Documents/advanced data analysis/project/clients.csv")
head(clients)
summary(clients)

# Check for missing values
sum(is.na(clients))

# Delete month variable
clients = clients[-1]
head(clients)

# Group categorical variable product_type
clients <- clients %>%
  mutate(product_type = fct_lump_n(clients$product_type, n = 4))
plot(clients$product_type)

# Create dummy variables for categorical variables
clients <- dummy_cols(clients, select_columns = "sex")
clients <- dummy_cols(clients, select_columns = "education")
clients <- dummy_cols(clients, select_columns = "product_type")
clients <- dummy_cols(clients, select_columns = "region")
clients <- dummy_cols(clients, select_columns = "family_status")
clients <- dummy_cols(clients, select_columns = "phone_operator")

# Transform numerical variables
clients$log_credit_amount <- log(clients$credit_amount)
clients$log_age <- log(clients$age)
clients$log_income <- log(clients$income)

# Visualize transformed variables
hist(clients$log_credit_amount)
hist(clients$log_age)
hist(clients$log_income)

# Get rid of unnecessary variables
clients <- clients[-c(1, 3:6, 8:11)]
head(clients)

# Split data into train and test sets
set.seed(123)
s <- sample(nrow(clients), 0.8 * nrow(clients))
clients_train <- clients[s, ]
clients_test <- clients[-s, ]

# Upsampling class 1 from bad client
trainup <- upSample(x = clients_train, y = as.factor(clients_train$bad_client_target))
table(trainup$bad_client_target)

# Separating X's and Y's as matrices
xTrain <- as.matrix(trainup[, -c(4, 32)])
yTrain <- as.matrix(trainup[, 4])
xTest <- as.matrix(clients_test[, -c(4, 32)])
yTest <- as.matrix(clients_test[, 4])

# Fit Lasso Logistic Model
fitLasso <- cv.glmnet(xTrain, yTrain, family = "binomial", alpha = 1)
plot(fitLasso)
ideal_lambda <- fitLasso$lambda.1se
fitLasso <- glmnet(xTrain, yTrain, family = "binomial", alpha = 1, lambda = ideal_lambda)
fitLasso
fitLasso$beta

# Predictions and RMSE
lassoPredTrain <- predict(fitLasso, xTrain, type = "response")
rmseLassoTrain <- sqrt(mean((lassoPredTrain - yTrain)^2))
lassoPredTest <- predict(fitLasso, xTest, type = "response")
rmseLassoTest <- sqrt(mean((lassoPredTest - yTest)^2))
rmseLassoTrain
rmseLassoTest

# Evaluate metrics
predict_classes <- ifelse(lassoPredTest > 0.5, 1, 0)
predict_classes <- as.factor(predict_classes)
yTest <- as.factor(yTest)
confusionMatrix(predict_classes, yTest)

# Downsampling class 0 from bad client
traindown <- downSample(x = clients_train, y = as.factor(clients_train$bad_client_target))
table(traindown$bad_client_target)

# Fit Lasso Logistic Model on downsampled data
fitLasso <- cv.glmnet(xTrain, yTrain, family = "binomial", alpha = 1)
plot(fitLasso)
ideal_lambda <- fitLasso$lambda.1se
fitLasso <- glmnet(xTrain, yTrain, family = "binomial", alpha = 1, lambda = ideal_lambda)

# Relaxed Lasso with Downsample
relaxedLasso <- cv.glmnet(xTrain, yTrain, family = "binomial", relax = TRUE)
relaxedLasso
plot(relaxedLasso)
relaxedLasso <- glmnet(xTrain, yTrain, family = "binomial", relax = TRUE, lambda = 0.031)

# Correspondence Analysis
prodEduTable <- table(clients$product_type, clients$education)
result2 <- CA(prodEduTable)
summary(result2)
mosaicplot(prodEduTable, shade = TRUE, xlab = "Product Type", ylab = "Education", main = "Product vs Education Mosaic Plot")

# Factor Analysis (PCA)
clients_X <- clients[c(3:4, 8:9, 12:13, 15:17)]
clients_y <- clients$bad_client_target
pca_cX <- prcomp(clients_X, scale. = TRUE)
summary(pca_cX)
plot(pca_cX)

# Factor analysis
pca_cX_4 <- principal(clients_X, nfactors = 4, rotate = "varimax")
print(pca_cX_4$loadings, cutoff=.4, sort=TRUE)

# Logistic Regression with PCA factors
pc_data <- as.data.frame(pca_cX_4$scores)
pc_data$y <- clients$bad_client_target
colnames(pc_data) <- c("credit_account_basics", "income", "family_relationship", "personal_info", "is_bad_customer")
set.seed(123)
s <- sample(nrow(pc_data), 0.8 * nrow(pc_data))
dsTrain <- pc_data[s, ]
dsTest <- pc_data[-s, ]

logistic_model <- glm(is_bad_customer ~ ., data = dsTrain, family = binomial(link = 'logit'))
summary(logistic_model)

# Prediction and Evaluation
pred <- predict(logistic_model, newdata = dsTest, type = 'response')
pred <- ifelse(pred > 0.5, 1, 0)
confusionMatrix(as.factor(pred), as.factor(dsTest$is_bad_customer))

# ROC and AUC
library(ROCR)
p <- predict(logistic_model, newdata = dsTest, type = "response")
pr <- prediction(p, dsTest$is_bad_customer)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

