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

library(ROCR)
library(pROC)

# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(as.numeric(nb_model_predict_test, bank_nb_test$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2

#optimal cut-off using Youden's index
nb_model_predict_test <- as.numeric(nb_model_predict_test)

pROC::coords(r, x = "best", input = "threshold", best.method = "youden")

r<- pROC::roc(bank_nb_test$bk, nb_model_predict_test, plot=TRUE,print.auc = TRUE)
