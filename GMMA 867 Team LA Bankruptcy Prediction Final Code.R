# Load required libraries 

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
library(ROCR)
library(pROC)
library(DMwR)
library(Rcpp)
library(rpart)
library(rpart.plot)
library(randomForest)
library(naivebayes)
library(psych)
library(e1071)
library(MASS)
library(devtools)
library(ggord)
library(neuralnet)

theme_set(theme_bw())
options(scipen = 999)

#### Data pre-processing ####

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

####Exploratory Plots####

#correlation plot
corrplot(cor(bank, use = "complete.obs"), method = "number")

#histograms
#1. EPS
eps <- ggplot(bank, aes(x=eps)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-15,15)
eps

#2. Liquidity
liq <- ggplot(bank, aes(x=liquidity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
liq

#3. profitability
prof <- ggplot(bank, aes(x=profitability)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
prof

#4. productivity
prod <- ggplot(bank, aes(x=productivity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
prod

#5. Leverage Ratio
lr <- ggplot(bank, aes(x=leverage_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
lr

#6. Asset Turnover
at <- ggplot(bank, aes(asset_turnover)) +   
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-0,5)
at

#7. Operational Margin
om <- ggplot(bank, aes(operational_margin)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
om

#8. Return on Equity
roe <- ggplot(bank, aes(return_on_equity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
roe

#9. Market Book Ratio
br <- ggplot(bank, aes(market_book_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-1000,1000)
br

#10. Assets Growth
ag <- ggplot(bank, aes(x=assets_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
ag

#11. Sales Growth
sg <- ggplot(bank, aes(x=sales_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
sg

#12. Employee Growth
eg <- ggplot(bank, aes(x=employee_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
eg

figure1 <- ggarrange(eps, liq, prof, 
                     prod, lr, at,
                     om, roe, br,
                     ag,sg,eg,
                     nrow = 4, ncol = 3)
figure1

#check the skewness of data
bank %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

#check for missing values
colSums(is.na(bank))

# This cleans the entire original data set (not split)
cleanedBank <-drop_na(bank)
colSums(is.na(cleanedBank))  

# Add an index just for programming purposes 
Obs <- 1:81204
cleanedBank$Obs <-Obs

# Split data between test and train sets
# Set seed so it can be repeated
set.seed(3141)

# Randomly sample 70% percent of the cleaned data set then arrange in order
trainBank <-sample_n(cleanedBank, floor(0.7*81204))
trainBank <-arrange(trainBank, Obs)
trainBank <- dplyr::select(trainBank, -Obs)

# The remaining data is used for the test set then arranged in order
testBank<-anti_join(cleanedBank, trainBank)
testBank<-arrange(testBank, Obs)
testBank <- dplyr::select(testBank, -Obs)

# SMOTE (Synthetic Minority Oversampling Technique)

require(devtools)

trainBank$bk <- as.factor(trainBank$bk)
smote_train_dataset <- as.data.frame(trainBank)
trainBank_SMOTE <-  SMOTE(bk ~., smote_train_dataset, perc.over = 10, perc.under = 1000, k=5)
table(trainBank_SMOTE$bk)

trainBank_SMOTE %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

########################### Altman Z-Score ###########################

#* Altman Z-Score = 1.2X1 + 1.4X2 + 3.3X3 + 0.6X4 + 1.0X5
#* X1 = liquidity
#* X2 = profitability
#* X3 = productivity
#* X4 = ??? Not sure what X4 is right now but using leverage ratio for now
#* X5 = asset_turnover

altman <- 1.2*(trainBank_SMOTE$liquidity) + 1.4*(trainBank_SMOTE$profitability) + 
  3.3*(trainBank_SMOTE$productivity) + 0.6*(trainBank_SMOTE$leverage_ratio) +
  1.0*(trainBank_SMOTE$asset_turnover)

#Perform prediction on the training data

altmanTrainingPredict <- altman

for (val in 1:701)
{
  if (altman[val] > 2.4)
  {
    altmanTrainingPredict[val] <-0
  }
  else
  {
    altmanTrainingPredict[val] <-1
    
  }
}

altmanTrainingPredict

# Altman Z-Score = 1.2X1 + 1.4X2 + 3.3X3 + 0.6X4 + 1.0X5
# X1 = liquidity
# X2 = profitability
# X3 = productivity
# X4 = leverage ratio
# X5 = asset_turnover

altmanTest <- 1.2*(testBank$liquidity) + 1.4*(testBank$profitability) + 
  3.3*(testBank$productivity) + 0.6*(testBank$leverage_ratio) +
  1.0*(testBank$asset_turnover)

# Perform prediction on the testing data

altmanTestPredict <- altmanTest

for (val in 1:24362)
{
  if (altmanTest[val] > 2.4)
  {
    altmanTestPredict[val] <-0
  }
  else
  {
    altmanTestPredict[val] <-1
    
  }
}

altmanTestPredict

# Confusion matrix
confusionMatrix(as.factor(altmanTestPredict), as.factor(testBank$bk))

par(pty = "s")

# ROC Curve
pROC::roc(testBank$bk, altmanTestPredict, plot=TRUE, print.auc = TRUE)

# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(altmanTestPredict, testBank$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2

par(pty = "m")

########################### Logistic Regression ###########################

logit <- glm(bk~eps+liquidity+profitability+productivity+leverage_ratio+asset_turnover+operational_margin
             +return_on_equity+market_book_ratio+assets_growth+sales_growth+employee_growth,
             data = trainBank_SMOTE,family = "binomial")
summary(logit)

predict_glm <- predict(logit,testBank,type="response")
predict_glm
predict_glm_class <- as.factor(ifelse(predict_glm > 0.5, 1,0))
predict_glm_class
table(predict_glm_class,testBank$bk)
confusionMatrix(predict_glm_class, reference = as.factor(testBank$bk))

logit_predict <- prediction(as.numeric(predict_glm),testBank$bk)
perf_logit <- performance(logit_predict,"tpr","fpr")
plot(perf_logit,colorize=TRUE)

auc_logit <- performance(logit_predict,"auc")
auc_logistics <- as.numeric(auc_logit@y.values)
auc_logistics

test_logistics <- as.numeric(predict_glm)
r <- pROC::roc(testBank$bk,test_logistics,plot=TRUE,print.auc=TRUE)
pROC::coords(r,"best")
pROC::coords(r,x="best",input="threshold",best.method="youden")

########################### KNN Model ###########################

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
r<- pROC::roc(testBank$bk, test_predknn, plot=TRUE,print.auc = TRUE)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")

########################### Discriminant Analysis ########################### 

ldaModel1 <-lda(bk ~., trainBank_SMOTE)
ldaModel1
plot(ldaModel1)
attributes(ldaModel1)
ldaModel1$counts
ldaModel1$lev
plot(ldaModel1)

lda1P <-predict(ldaModel1, trainBank_SMOTE)
ldahist(data = lda1P$x[,1], g = trainBank_SMOTE$bk)

# Confusion Matrix - Training Data
ldaP1 <-predict(ldaModel1, trainBank_SMOTE)$class
ldaTable <- table(Predicted = ldaP1, Actual = trainBank_SMOTE$bk)
ldaTable
confusionMatrix(ldaP1, as.factor(trainBank_SMOTE$bk))

accuracy <- sum(diag(ldaTable)/sum(ldaTable))
accuracy

#Confusion Matrix - Testing Data
ldaP2 <- predict(ldaModel1, testBank)$class
ldaTable2 <- table(Predicted = ldaP2, Actual = testBank$bk)
ldaTable2
confusionMatrix(ldaP2, as.factor(testBank$bk))

accuracy2 <- sum(diag(ldaTable2)/sum(ldaTable2))
accuracy2

# Partition Plots

#library(klaR)
#partimat(bk ~.,data = trainBank_SMOTE, method="lda", main = "Partition Plots")

# pty sets the aspect ratio of the plot region. Two options:
# s" - creates a square plotting region
# "m" - (the default) creates a maximal plotting region
# This will be switched back to "m'" when finished
par(pty = "s")

actualValuesLDA <-testBank$bk
predictedValuesLDA <- as.numeric(ldaP2)

pROC::roc(actualValuesLDA, predictedValuesLDA, plot=TRUE, print.auc = TRUE)

# TPR vs FPR Plot and AUC

lda_predict <- prediction(as.numeric(predictedValuesLDA), actualValuesLDA)
perf_lda <- performance(lda_predict, "tpr", "fpr")
plot(perf_lda, colorize = TRUE)

auc_lda <- performance(lda_predict, "auc")
auc_lda2 <- as.numeric(auc_lda@y.values)
auc_lda2

# pred <- ROCR::prediction(ldaModel1$prior[,2], trainBank$bk)
# perf <- ROCR::performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)

#ggord(ldaModel1, trainBank_SMOTE$bk)

#ggord(linear, training$Species, ylim = c(-10, 10))

# Quadratic Discriminant Analysis (QDA)
qdaModel <-qda(bk ~., data = trainBank_SMOTE)
qdaModel
#plot(qdaModel)

attributes(qdaModel)
qdaModel$counts
qdaModel$lev

qdaP <-predict(qdaModel, trainBank_SMOTE)
#qdahist(data = qdaP$x[,1], g = trainBank$bk)

#Confusion Matrix - Testing Data
qdaP1 <- predict(ldaModel1, testBank)$class
qdaTable1 <- table(Predicted = ldaP2, Actual = testBank$bk)
qdaTable1
confusionMatrix(qdaP1, as.factor(testBank$bk))

#accuracy2 <- sum(diag(ldaTable2)/sum(ldaTable2))
#accuracy2

actualValuesQDA <-testBank$bk
predictedValuesQDA <- as.numeric(qdaP1)

pROC::roc(actualValuesQDA, predictedValuesQDA, plot=TRUE, print.auc = TRUE)

# pty sets the aspect ratio of the plot region. Two options:
# s" - creates a square plotting region
# "m" - (the default) creates a maximal plotting region
# This will be switched back to "m'" when finished
par(pty = "m")

########################### SVM ########################### 

svmModel <-svm(bk ~., data = trainBank_SMOTE, kernel = "linear", cost = 1, scale = TRUE)
print(svmModel)
summary(svmModel)

# Training
svmPredictTrain<-predict(svmModel, trainBank_SMOTE)

# Confusion matrix
confusionMatrix(svmPredictTrain, as.factor(trainBank_SMOTE$bk))

# ROC Curve
pROC::roc(trainBank_SMOTE$bk, as.numeric(svmPredictTrain), plot=TRUE, print.auc = TRUE)

# Testing
svmPredictTest<-predict(svmModel, testBank)

# Confusion matrix
confusionMatrix(svmPredictTest, as.factor(testBank$bk))

# pty sets the aspect ratio of the plot region. Two options:
# s" - creates a square plotting region
# "m" - (the default) creates a maximal plotting region
# This will be switched back to "m'" when finished
par(pty = "s")

# ROC Curve
pROC::roc(testBank$bk, as.numeric(svmPredictTest), plot=TRUE, print.auc = TRUE)

# SVM - TPR vs FPR Plot and AUC

svm_predict <- prediction(as.numeric(svmPredictTest), testBank$bk)
perf_svm <- performance(svm_predict, "tpr", "fpr")
plot(perf_svm, colorize = TRUE)

auc_svm <- performance(svm_predict, "auc")
auc_svm2 <- as.numeric(auc_svm@y.values)
auc_svm2

# Support Vector Machines Model 2
svmModel1 <-svm(bk ~., data = trainBank_SMOTE, kernel = "polynomial", cost = 1, scale = TRUE)
print(svmModel1)
summary(svmModel1)

# Training
svmPredictTrain<-predict(svmModel1, trainBank_SMOTE)

# Confusion matrix
confusionMatrix(svmPredictTrain, as.factor(trainBank_SMOTE$bk))

# ROC Curve
pROC::roc(trainBank_SMOTE$bk, as.numeric(svmPredictTrain), plot=TRUE, print.auc = TRUE)

# Testing
svmPredictTest<-predict(svmModel1, testBank)

# Confusion matrix
confusionMatrix(svmPredictTest, as.factor(testBank$bk))

# ROC Curve
pROC::roc(testBank$bk, as.numeric(svmPredictTest), plot=TRUE, print.auc = TRUE)

# Support Vector Machines Model
svmModel1 <-svm(bk ~., data = trainBank_SMOTE, kernel = "sigmoid", cost = 1, scale = TRUE)
print(svmModel1)
summary(svmModel1)

# Training
svmPredictTrain<-predict(svmModel1, trainBank_SMOTE)

# Confusion matrix
confusionMatrix(svmPredictTrain, as.factor(trainBank_SMOTE$bk))

# ROC Curve
pROC::roc(trainBank_SMOTE$bk, as.numeric(svmPredictTrain), plot=TRUE, print.auc = TRUE)

# Testing
svmPredictTest<-predict(svmModel1, testBank)

# Confusion matrix
confusionMatrix(svmPredictTest, as.factor(testBank$bk))

# ROC Curve
pROC::roc(testBank$bk, as.numeric(svmPredictTest), plot=TRUE, print.auc = TRUE)

# pty sets the aspect ratio of the plot region. Two options:
# s" - creates a square plotting region
# "m" - (the default) creates a maximal plotting region
# This will be switched back to "m'" when finished
par(pty = "m")
 
# library(ROCR)
# #data(ROCR.simple)
# pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
# pred2 <- prediction(abs(ROCR.simple$predictions + 
#                           rnorm(length(ROCR.simple$predictions), 0, 0.1)), 
#                     ROCR.simple$labels)
# perf <- performance( pred, "tpr", "fpr" )
# perf2 <- performance(pred2, "tpr", "fpr")
# plot( perf, colorize = TRUE)
# plot(perf2, add = TRUE, colorize = TRUE)

# library(ROCR)
# data(ROCR.hiv)
# x   <- prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
# ROC <- performance(x, "tpr", "fpr")
# plot(ROC, col = as.list(1:10))


########################### Trees ###########################

####Classification tree using all predictors####

tree_Bank <- rpart(formula = bk~., data=trainBank_SMOTE, method = "class", minbucket = 50, maxdepth = 7)
rpart.plot(tree_Bank)
plotcp(tree_Bank)
printcp(tree_Bank)

#Prediction Accuracy - Classification Tree
pred_tree_Bank <- predict(tree_Bank, newdata = testBank, type = "class")
confusionMatrix(pred_tree_Bank, reference = as.factor(testBank$bk))

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

####Random Forest####

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

########################### Naive Bayes ###########################

bank_nb_train <- trainBank_SMOTE
bank_nb_test <- testBank
bank_nb_train$bk <- as.factor(bank_nb_train$bk)
bank_nb_test$bk <- as.factor(bank_nb_test$bk)

nb_model <- naive_bayes(bk ~ ., data = bank_nb_train, usekernel=F)
plot(nb_model)
print(nb_model)

nb_model_predict <- predict(nb_model, bank_nb_train, type = 'class')
head(cbind(nb_model_predict, bank_nb_train))
nb_table <- table(nb_model_predict, bank_nb_train$bk, dnn=c("Prediction","Actual"))
nb_table
1 - sum(diag(nb_table))/sum(nb_table)

nb_model_predict_test <- predict(nb_model, bank_nb_test, type = 'class')
head(cbind(nb_model_predict_test, bank_nb_test))
nb_table2 <- table(nb_model_predict_test, bank_nb_test$bk, dnn=c("Prediction","Actual"))
nb_table2
1 - sum(diag(nb_table2)) / sum(nb_table2)

NB_train <- table(nb_model_predict, bank_nb_train$bk)
confusionMatrix(NB_train)

NB_test <- table(nb_model_predict_test, bank_nb_test$bk)
confusionMatrix(NB_test)

# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(as.numeric(nb_model_predict_test), bank_nb_test$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2

#optimal cut-off using Youden's index
nb_model_predict_test <- as.numeric(nb_model_predict_test)

r<- pROC::roc(bank_nb_test$bk, nb_model_predict_test, plot=TRUE,print.auc = TRUE)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")

########################### Neural Network ###########################

options(scipen = 999)

#import data
bank <- read_excel(file.choose())

colnames(bank) <- c("eps", "liquidity", "profitability", "productivity",
                    "leverage_ratio", "asset_turnover", "operational_margin",
                    "return_on_equity", "market_book_ratio", "assets_growth",
                    "sales_growth", "employee_growth", "bk")

bank <-drop_na(bank)

#winsorize 0.5 percentile and 0.95 percentile data

wbank <- winsor(bank, trim = 0.005, na.rm = T)
wbank <- as.data.frame(wbank)
summary(wbank)

#Normalize data to [-1,1] scale

normalize <- function(x){
  return(2*(x-max(x))/(max(x)-min(x))+1)
}

scaled_bank <- as.data.frame(apply(wbank[,-13], 2, function(x) normalize(x)))
scaled_bank <- cbind(scaled_bank, bk= bank$bk)
summary(scaled_bank)

sample <- sample.int(n=nrow(scaled_bank), size = floor(0.7*nrow(scaled_bank)), replace = F)
train <- scaled_bank[sample,]
test <- scaled_bank[-sample,]
train$bk <- as.factor(train$bk)

balanced_data <- SMOTE(bk ~., train, perc.over = 10, perc.under = 1000, k=5)
balanced_data$bk <- as.numeric(as.character(balanced_data$bk))

balanced_data %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

summary(balanced_data)
str(balanced_data)

nn = neuralnet(bk ~ ., data = balanced_data, 
               hidden = 6, 
               linear.output = FALSE, 
               threshold = 0.01,
               stepmax = 1000000,
)
plot(nn)

predict_nn <- compute(nn, test)
predict_nn$net.result

predict_nn_class <- ifelse(predict_nn$net.result>0.52322, 1, 0)
predict_nn_class

t <- table(predict_nn_class, test$bk)
confusionMatrix(t)

#TPR vs FPR Plot and AUC
nn.predict <- prediction(predict_nn$net.result, test$bk)
perf_nn <- performance(nn.predict, "tpr", "fpr")
plot(perf_nn, colorize = TRUE)

auc.tmp <- performance(nn.predict, "auc")
auc <- as.numeric(auc.tmp@y.values)
auc

#optimal cut-off using Youden's index
ROC <- plot.roc(test$bk, predict_nn$net.result)
coords(ROC, "b", ret = "t", best.method="youden")


