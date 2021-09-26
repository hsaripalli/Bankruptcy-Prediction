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
trainBank <- select(trainBank, -Obs)

# The remaining data is used for the test set then arranged in order
testBank<-anti_join(cleanedBank, trainBank)
testBank<-arrange(testBank, Obs)
testBank <- select(testBank, -Obs)

# Checks for missing data 
# md.pattern(cleanedBank)
# md.pattern(trainBank)
# md.pattern(testBank)

# SMOTE (Synthetic Minority Oversampling Technique)

# install.packages("devtools")
require(devtools)
# install_version("DMwR", version = "0.4.1", repos = "http://cran.us.r-project.org")
library(DMwR)

trainBank$bk <- as.factor(trainBank$bk)
smote_train_dataset <- as.data.frame(trainBank)
trainBank_SMOTE <-  SMOTE(bk ~., smote_train_dataset, perc.over = 10, perc.under = 1000, k=5)
table(trainBank_SMOTE$bk)

trainBank_SMOTE %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

###### !!!!!!!!!!!!!!!!!!! USE THIS FOR MODEL TRAINING: trainBank_SMOTE <------ #########

library(ROCR)
library(pROC)

####### USE THIS FOR PERFORMANCE EVALUATION ON BOTH TRAINING AND TESTING DATA ####
####### NEED: CONFUSION MATRIX, AUC CURVE, AUC VALUE ##########

# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(nb_model_predict_test, bank_nb_test$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2

#optimal cut-off using Youden's index
nb_model_predict_test <- as.numeric(nb_model_predict_test)

pROC::coords(r, "best")
pROC::coords(r, x = "best", input = "threshold", best.method = "youden")

r<- pROC::roc(bank_nb_test$bk, nb_model_predict_test, plot=TRUE,print.auc = TRUE)


########################### Logistic Regression ####################################

log <- glm(bk ~., data = trainBank_SMOTE, family = binomial)
summary(log)

predict_glm <- predict(log, testBank, type = "response")
predict_glm

predict_glm_class <- as.factor(ifelse(predict_glm > 0.5, 1,0))
confusionMatrix(predict_glm_class, reference = as.factor(testBank$bk))


########## KNN Model ##########

trctrl <- trainControl(method = "cv", number = 10)

knn_fit <- train(bk ~., data = trainBank_SMOTE, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"), 
                 tuneLength = 10)
print(knn_fit)

#<<<<<<< HEAD
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

library(ROCR)

#TPR vs FPR Plot and AUC

tree_predict <- prediction(as.numeric(pred_tree_Bank), testBank$bk)
perf_tree <- performance(tree_predict, "tpr", "fpr")
plot(perf_tree, colorize = TRUE)

auc.tmp <- performance(tree_predict, "auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#optimal cut-off using Youden's index
pred_tree_Bank <- as.numeric(pred_tree_Bank)

r<- pROC::roc(testBank$bk, pred_tree_Bank, plot=TRUE,print.auc = TRUE)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")


####Random Forests####
library(randomForest)

rf_Bank <- randomForest(as.factor(bk)~., data = trainBank_SMOTE,
                        ntree = 500,
                        mtry = 3,
                        importance = TRUE)

#Variable importance plot
varImpPlot(rf_Bank, type = 1, main= "Variable Importance Plot")

#Prediction Accuracy - Random Forest
pred_rf_Bank <- predict(rf_Bank, testBank)
confusionMatrix(pred_rf_Bank, reference = as.factor(testBank$bk))

#Tune mtry
tune_rf <- tuneRF(trainBank_SMOTE[,-13], trainBank_SMOTE$bk,stepFactor = 0.5,
                  plot = TRUE, ntreeTry = 500, trace = TRUE, improve = 0.05)

#TPR vs FPR Plot and AUC

rf_predict <- prediction(as.numeric(pred_rf_Bank), testBank$bk)
perf_rf <- performance(rf_predict, "tpr", "fpr")
plot(perf_rf, colorize = TRUE)

auc.tmp <- performance(rf_predict, "auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#optimal cut-off using Youden's index
pred_rf_Bank <- as.numeric(pred_rf_Bank)

r<- pROC::roc(testBank$bk, pred_rf_Bank, plot=TRUE,print.auc = TRUE)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")


########## KNN Model ##########
#=======
testBank$bk <- as.factor(testBank$bk)

test_predknn <- predict(knn_fit, newdata = testBank)
confusionMatrix(test_predknn, testBank$bk)

knn_predict <- prediction(as.numeric(test_predknn), testBank$bk)
perf_knn <- performance(knn_predict, "tpr", "fpr")
plot(perf_knn, colorize = TRUE)

auc_knn <- performance(knn_predict, "auc")
auc_knn2 <- as.numeric(auc_knn@y.values)
auc_knn2

#optimal cut-off using Youden's index
test_predknn <- as.numeric(test_predknn)
#>>>>>>> 6e302a57460b6fdc6688cc79c773a4ef132c9ebf

r<- pROC::roc(testBank$bk, test_predknn, plot=TRUE,print.auc = TRUE)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")

