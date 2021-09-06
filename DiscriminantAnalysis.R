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

training<-bank[88631:92872,]
testing<-bank[0:88630,]

training <-drop_na(training)
tesing <-drop_na(testing)


# Linear Discriminant Analysis (LDA)
ldaModel <-lda(bk ~., data = training)
ldaModel
attributes(ldaModel)
ldaModel$counts
ldaModel$lev

ldaP <-predict(ldaModel, training)
ldahist(data = ldaP$x[,1], g = training$bk)

#****
# Trying out different test / train sets
#****


# Confusion Matrix - Training Data
# ldaP1 <-predict(ldaModel, training)$class
# ldaTable <- table(Predicted = ldaP1, Actual = training$bk)
# ldaTable
# 
# accuracy <- sum(diag(ldaTable)/sum(ldaTable))
# accuracy
# 
# #Confusion Matrix - Testing Data
# ldaP2 <- predict(ldaModel, testing)$class
# ldaTable2 <- table(Predicted = ldaP2, Actual = testing$bk)
# ldaTable2
# 
# accuracy2 <- sum(diag(ldaTable2)/sum(ldaTable2))
# accuracy2

#********
# Going with the orginal test / train set
#********

ldaModel1 <-lda(bk ~.-Obs, data = trainBank)
ldaModel1
attributes(ldaModel)
ldaModel1$counts
ldaModel1$lev

lda1P <-predict(ldaModel, trainBank)
ldahist(data = ldaP$x[,1], g = trainBank$bk)

# Confusion Matrix - Training Data
ldaP1 <-predict(ldaModel, trainBank)$class
ldaTable <- table(Predicted = ldaP1, Actual = trainBank$bk)
ldaTable

accuracy <- sum(diag(ldaTable)/sum(ldaTable))
accuracy

#Confusion Matrix - Testing Data
ldaP2 <- predict(ldaModel, testBank)$class
ldaTable2 <- table(Predicted = ldaP2, Actual = testBank$bk)
ldaTable2

accuracy2 <- sum(diag(ldaTable2)/sum(ldaTable2))
accuracy2

#install.packages("devtools")
library(devtools)
#install_github("fawda123/ggord")
library(ggord)

#ggord::ggord(ldaModel, trainBank$bk)

# Quadratic Discriminant Analysis (QDA)
# qdaModel <-qda(bk ~., data = training)
# qdaModel
# attributes(qdaModel)
# qdaModel$counts
# qdaModel$lev
# 
# qdaP <-predict(qdaModel, training)
#qdahist(data = qdaP$x[,1], g = trainBank$bk)

