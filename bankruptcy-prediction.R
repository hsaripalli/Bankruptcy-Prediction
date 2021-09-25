library(dplyr)
library(readxl)
library(caret)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(mice)
library(tidyr)
library(DMwR)
library(UBL)
library(car)
library(outliers)

theme_set(theme_bw())
options(scipen = 999)

#import data
bank <- read_excel(file.choose())

colnames(bank) <- c("eps", "liquidity", "profitability", "productivity",
                    "leverage_ratio", "asset_turnover", "operational_margin",
                    "return_on_equity", "market_book_ratio", "assets_growth",
                    "sales_growth", "employee_growth", "bk")

#examine data
str(bank)
summary(bank)
head(bank)

#check the skewness of data
bank %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

#check for missing values
colSums(is.na(bank))


#*******************************************************
# Delete missing row code section
# 
# Comment this section out if you do not want to do this
#*******************************************************


# This cleans the entire original data set (not split)
cleanedBank <-drop_na(bank)
colSums(is.na(cleanedBank))  


# Add an index just for programming purposes (will have to be ignored for data science)
Obs <- 1:81204
cleanedBank$Obs <-Obs

# **************************************
# Split data between test and train sets
# **************************************

# Set seed so it can be repeated
set.seed(3141)

# Randomly sample 70% percent of the cleaned data set then arrange in order
trainBank <-sample_n(cleanedBank, floor(0.7*81204))
trainBank <-arrange(trainBank, Obs)

# The remaining data is used for the test set then arranged in order
testBank<-anti_join(cleanedBank, trainBank)
testBank<-arrange(testBank, Obs)


# Checks for missing data 
# md.pattern(cleanedBank)
# md.pattern(trainBank)
# md.pattern(testBank)

# SMOTE (Synthetic Minority Oversampling Technique)

#install.packages("devtools")
require(devtools)
#install_version("DMwR", version = "0.4.1", repos = "http://cran.us.r-project.org")
library(DMwR)

trainBank$bk <- as.factor(trainBank$bk)
smote_train_dataset <- as.data.frame(trainBank)
trainBank_SMOTE <-  SMOTE(bk ~., smote_train_dataset, perc.over = 10, perc.under = 1000, k=5)
table(trainBank_SMOTE$bk)

trainBank_SMOTE %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)


########################### Naive Bayes classification ####################################
# Used this source as reference: https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
# Another source: https://www.learnbymarketing.com/tutorials/naive-bayes-in-r/ 

library(naivebayes)
library(psych)
library(e1071)

bank_nb_train <- trainBank_SMOTE
bank_nb_test <- testBank
bank_nb_train$bk <- as.factor(bank_nb_train$bk)
bank_nb_test$bk <- as.factor(bank_nb_test$bk)

nb_model <- naive_bayes(bk ~ ., data = bank_nb_train, usekernel=T)
plot(nb_model)
print(nb_model)

nb_model_predict <- predict(nb_model, bank_nb_train, type = 'class')
head(cbind(nb_model_predict, bank_nb_train))
nb_table <- table(nb_model_predict, bank_nb_train$bk, dnn=c("Prediction","Actual"))
nb_table
1 - sum(diag(nb_table))/sum(nb_table)

nb_model_predict_test <- predict(nb_model, bank_nb_test)
head(cbind(nb_model_predict_test, bank_nb_test))
nb_table2 <- table(nb_model_predict_test, bank_nb_test$bk, dnn=c("Prediction","Actual"))
nb_table2
1 - sum(diag(nb_table2)) / sum(nb_table2)

NB_train <- table(nb_model_predict, bank_nb_train$bk)
confusionMatrix(NB_train)

NB_test <- table(nb_model_predict_test, bank_nb_test$bk)
confusionMatrix(NB_test)

library(ROCR)

#TPR vs FPR Plot and AUC
nb_model_predict_test <- list(nb_model_predict_test)
bank_nb_test$bk <- list(bank_nb_test$bk)

nb_predict <- prediction(nb_model_predict_test, bank_nb_test$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc.tmp <- performance(nn.predict, "auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#optimal cut-off using Youden's index
ROC <- plot.roc(test$bk, predict_nn$net.result)
coords(ROC, "b", ret = "t", best.method="youden")


########################### Logistic Regression ####################################

log <- glm(bk ~., data = balanced_data, family = binomial)
summary(log)

predict_glm <- predict(log, test, type = "response")
predict_glm

predict_glm_class <- as.factor(ifelse(predict_glm > 0.5, 1,0))
confusionMatrix(predict_glm_class, reference = as.factor(test$bk))


########## KNN Model ##########

trctrl <- trainControl(method = "cv", number = 10)

knn_fit <- train(bk ~., data = trainBank_SMOTE, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"), 
                 tuneLength = 10)
print(knn_fit)

test_pred <- predict(knn_fit, newdata = testBank)
confusionMatrix(test_pred, testBank$bk)


########################### Trees ####################################
library(Rcpp)
library(rpart)
library(rpart.plot)

# This cleans the entire original data set (not split)
#cleanedBank <-drop_na(bank)
#colSums(is.na(cleanedBank))  
#str(cleanedBank)

#convert bk to factor
#cleanedBank$bk <- as.factor(cleanedBank$bk)
#str(cleanedBank)

# Set seed so it can be repeated
set.seed(3141)

# Randomly sample 70% percent of the cleaned data set then arrange in order
trainBank_SMOTE <-sample_n(cleanedBank, floor(0.7*81204))
str(trainBank)

# The remaining data is used for the test set then arranged in order
testBank<-anti_join(cleanedBank, trainBank)

####Classification tree using all predictors####

tree_Bank <- rpart(formula = bk~., data=trainBank_SMOTE, method = "class", minbucket = 50, maxdepth = 7)
rpart.plot(tree_Bank)
plotcp(tree_Bank)
printcp(tree_Bank)

#Prediction Accuracy - Classification Tree
pred_tree_Bank <- predict(tree_Bank, newdata = testBank, type = "class")
confusionMatrix(pred_tree_Bank, reference = as.factor(testBank$bk))


####Random Forests####
library(randomForest)

rf_Bank <- randomForest(as.factor(bk)~., data = trainBank_SMOTE,
                        ntree = 500,
                        mtry = 3,
                        importance = TRUE)

#Variable importance plot
varImpPlot(rf_Bank, type = 1, main= "Variable Importance Plot")
?varImpPlot()
#Prediction Accuracy - Random Forest
pred_rf_Bank <- predict(rf_Bank, testBank)
confusionMatrix(pred_rf_Bank, reference = as.factor(testBank$bk))

#Tune mtry
tune_rf <- tuneRF(trainBank_SMOTE[,-13], trainBank_SMOTE$bk,stepFactor = 0.5,
                  plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.05)


####Boosting####
library(adabag)

boost_Bank <- boosting(bk~., data = trainBank)

#Prediction Accuracy - Boosting
pred_boost_Bank <- predict(boost_Bank, testBank)
confusionMatrix(pred_boost_Bank, testBank$bk)


####Boosting 2####
library(gbm)

boost_Bank2 <- gbm(bk~., data=trainBank, distribution= "Bernuolli",
                   n.trees = 5000, interaction.depth =4)


pred_boost_Bank2 <- predict(boost_Bank2, testBank)

########## KNN Model ##########

trctrl <- trainControl(method = "cv", number = 10)

knn_fit <- train(bk ~., data = trainBank_SMOTE, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"), 
                 tuneLength = 10)
print(knn_fit)

test_pred <- predict(knn_fit, newdata = testBank)
confusionMatrix(test_pred, testBank$bk)
