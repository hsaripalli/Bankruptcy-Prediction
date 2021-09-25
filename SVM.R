# ************************************************************************
# SVM.R -  Support Vector Machines (SVM) file
# 
# This file runs the SVM model on the data and produces associated plots
# ************************************************************************



# ********************************************************************
# Load the main bankruptcy-prediction.R file
# You must choose this main R file and then choose the Excel data file
# ********************************************************************
source(file.choose())


# Load required libraries
library(e1071)

trainBank_SMOTE <-dplyr::select(trainBank_SMOTE, -Obs)
testBank <-dplyr::select(testBank, -Obs)

# Support Vector Machines Model
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






# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(as.numeric(svmPredictTest), testBank$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2





# Support Vector Machines Model
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




# 
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
# 
# 
# library(ROCR)
# data(ROCR.hiv)
# x   <- prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
# ROC <- performance(x, "tpr", "fpr")
# plot(ROC, col = as.list(1:10))
