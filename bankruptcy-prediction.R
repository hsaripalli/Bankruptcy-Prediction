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

# SMOTE - package not available; waiting for instruction from Prof. Scott 

# install.packages("devtools")
# require(devtools)
# install_version("DMwR", version = "0.4.1", repos = "http://cran.us.r-project.org")
# library(DMwR)

########################### Naive Bayes classification ####################################
# Used this source as reference: https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
# Another source: https://www.learnbymarketing.com/tutorials/naive-bayes-in-r/ 

library(naivebayes)
library(psych)
library(e1071)

bank_nb_train <- trainBank
bank_nb_test <- testBank
bank_nb_train$bk <- as.factor(bank_nb_train$bk)
bank_nb_test$bk <- as.factor(bank_nb_test$bk)

corrplot(cleanedBank)

nb_model <- naive_bayes(bk ~ ., data = bank_nb_train, usekernel = T, laplace=1)
plot(nb_model)

nb_model_predict <- predict(nb_model, bank_nb_train, type = 'class')
head(cbind(nb_model_predict, bank_nb_train))
nb_table <- table(nb_model_predict, bank_nb_train$bk, dnn=c("Prediction","Actual"))
nb_table
1 - sum(diag(nb_table))/sum(nb_table)

nb_model_predict_test <- predict(nb_model, bank_nb_test)
head(cbind(nb_model_predict_test, bank_nb_test))
nb_table2 <- table(nb_model_predict_test, bank_nb_test$bk)
1 - sum(diag(nb_table2)) / sum(nb_table2)


########################### Neural Networks ####################################



#wbank <- winsor(bank, trim = 0.005, na.rm = T)
#wbank <- as.data.frame(wbank)
#summary(wbank)


#wbank %>%
#count(bk) %>%
#mutate(percent = n/sum(n)*100)
#wbank <- drop_na(wbank)
#corrplot(cor(wbank, use = "complete.obs"), method = "number")
#includes missing data
# test train split 70%
#scaling data to [0,1] scale
#max = apply(wbank, 2, max, na.rm = TRUE)
#min = apply(wbank, 2, min, na.rm = TRUE)
#scaled_bank = as.data.frame(scale(wbank, center = min, scale = max-min))
#summary(scaled_bank)


bank <-drop_na(bank)
library(neuralnet)
#Noramizlie data to [-1,1] scale

normalize <- function(x){
  return(2*(x-max(x))/(max(x)-min(x))+1)
}


#summary(wbank)
scaled_bank <- as.data.frame(apply(bank[,-13], 2, function(x) normalize(x)))
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

nn = neuralnet(bk ~ ., data = balanced_data, hidden = c(11,6,3), linear.output = FALSE, act.fct = "logistic")
plot(nn)

predict_nn <- compute(nn, test)
predict_nn$net.result

predict_nn_class <- ifelse(predict_nn$net.result>0.52586, 1, 0)
predict_nn_class

t <- table(predict_nn_class, test$bk)
confusionMatrix(t)

########################### Logistic Regression ####################################

log <- glm(bk ~., data = balanced_data, family = binomial)
summary(log)

predict_glm <- predict(log, test, type = "response")
predict_glm

predict_glm_class <- as.factor(ifelse(predict_glm > 0.5, 1,0))
confusionMatrix(predict_glm_class, reference = as.factor(test$bk))


########################### Trees ####################################
library(Rcpp)
library(rpart)
library(rpart.plot)

# This cleans the entire original data set (not split)
cleanedBank <-drop_na(bank)
colSums(is.na(cleanedBank))  


# Add an index just for programming purposes (will have to be ignored for data science)
Obs <- 1:81204
cleanedBank$Obs <-Obs

#convert bk to factor
#cleanedBank$bk <- as.factor(cleanedBank$bk)
#str(cleanedBank)

#one-hot encoding for all categorical variables
dummy <- dummyVars("~.", cleanedBank)
cleanedBank <- data.frame(predict(dummy, cleanedBank))

# Set seed so it can be repeated
set.seed(3141)

# Randomly sample 70% percent of the cleaned data set then arrange in order
trainBank <-sample_n(cleanedBank, floor(0.7*81204))
trainBank <-arrange(trainBank, Obs)
str(trainBank)

# The remaining data is used for the test set then arranged in order
testBank<-anti_join(cleanedBank, trainBank)
testBank<-arrange(testBank, Obs)

#Classification tree using all predictors
tree_Bank <- rpart(formula = bk~., data=trainBank, method = "class", minbucket = 50, maxdepth = 7)
rpart.plot(tree_Bank)
plotcp(tree_Bank)
printcp(tree_Bank)
