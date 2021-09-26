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
# set.seed(3141)

# Randomly sample 70% percent of the cleaned data set then arrange in order
# trainBank_SMOTE <-sample_n(cleanedBank, floor(0.7*81204))
# str(trainBank)

# The remaining data is used for the test set then arranged in order
# testBank<-anti_join(cleanedBank, trainBank)

####Classification tree using all predictors####

tree_Bank <- rpart(formula = bk~., data=trainBank_SMOTE, method = "class", 
                   minbucket = 50, maxdepth = 7)
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
varImpPlot(rf_Bank, type = 1)

#Prediction Accuracy - Random Forest

testBank$bk <- as.factor(testBank$bk)

pred_rf_Bank <- predict(rf_Bank, testBank)
confusionMatrix(pred_rf_Bank, testBank$bk)

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

?gbm
boost_Bank2 <- gbm(bk~., data=trainBank, distribution= "bernoulli",
                   n.trees = 5000, interaction.depth =4)


pred_boost_Bank2 <- predict(boost_Bank2, testBank)
