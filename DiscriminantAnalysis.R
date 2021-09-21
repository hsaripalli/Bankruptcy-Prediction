# ************************************************************************
# DiscriminantAnalysis.R -  Discriminant Analysis file
# 
# This file runs the Discriminant Analysis model on the data and 
# produces associated plots (both LDA and QDA)
# ************************************************************************


# ********************************************************************
# Load the main bankruptcy-prediction.R file
# You must choose this main R file and then choose the Excel data file
# ********************************************************************
source(file.choose())

# Load required libraries
library(MASS)
library(pROC)
library(ROCR)


#********
# Going with the original test / train set
#********


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

#install.packages("devtools")
library(devtools)
#install_github("fawda123/ggord")
library(ggord)

# Partition Plots
#install.packages("klaR")

#library(klaR)
#partimat(bk ~.,data = trainBank_SMOTE, method="lda", main = "Partition Plots")

actualValuesLDA <-testBank$bk
predictedValuesLDA <- as.numeric(ldaP2)

pROC::roc(actualValuesLDA, predictedValuesLDA, plot=TRUE, print.auc = TRUE)





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

