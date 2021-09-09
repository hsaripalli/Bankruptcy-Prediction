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
install.packages("outliers")
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

nb_model <- naive_bayes(bk ~ ., data = bank_nb_train, usekernel = T, laplace=1)
plot(nb_model)

nb_model_predict <- predict(nb_model, bank_nb_train, type = 'class')
head(cbind(nb_model_predict, bank_nb_train))
nb_table <- table(nb_model_predict, bank_nb_train$bk, dnn=c("Prediction","Actual"))
nb_table
1 - sum(diag(nb_table))/sum(nb_table)

nb_model_predict_test <- predict(nb_model, bank_nb_test)
nb_table2 <- table(nb_model_predict_test, bank_nb_test$bk)
1 - sum(diag(nb_table2)) / sum(nb_table2)



########################### Neural Networks ####################################




#includes missing data
# test train split 70%


install.packages("neuralnet")
library(neuralnet)

#scaling data to [0,1] scale


max = apply(bank, 2, max, na.rm = TRUE)
min = apply(bank, 2, min, na.rm = TRUE)
scaled_bank = as.data.frame(scale(bank, center = min, scale = max-min))

scaled_bank$bk <- bank$bk
summary(scaled_bank)

scaled_bank <- drop_na(scaled_bank)

sample <- sample.int(n=nrow(scaled_bank), size = floor(0.7*nrow(scaled_bank)), replace = F)
train <- scaled_bank[sample,]
test <- scaled_bank[-sample,]

scaled_bank


nn = neuralnet(bk ~ ., data = cleaned_bank, hidden = c(3,2), linear.output = FALSE)
plot(nn)

predict <- compute(nn, scaled_bank)
predict

pred <- ifelse(predict$net.result>0.5, 1, 0)
pred

tab1 <- table(pred, scaled_bank$bk)
tab1

#correlation plot
corrplot(cor(bank, use = "complete.obs"), method = "number")

#histograms
#1. EPS
eps <- ggplot(bank, aes(x=eps)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-100,100) + 
  scale_y_log10()
eps

#2. Liquidity
liq <- ggplot(bank, aes(x=liquidity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-100,2) + 
  scale_y_log10()
liq

#3. profitability
prof <- ggplot(bank, aes(x=profitability)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-100,10) + 
  scale_y_log10()
prof

#4. productivity
prod <- ggplot(bank, aes(x=productivity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-20,5) + 
  scale_y_log10()
prod

#5. Leverage Ratio
lr <- ggplot(bank, aes(x=leverage_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-250,250) + 
  scale_y_log10()
lr

#6. Asset Turnover
at <- ggplot(bank, aes(asset_turnover)) +   
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-1,10) + 
  scale_y_log10()
at

#7. Operational Margin
om <- ggplot(bank, aes(operational_margin)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-200,2) + 
  scale_y_log10()
om

#8. Return on Equity
roe <- ggplot(bank, aes(return_on_equity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-20,5) + 
  scale_y_log10()
roe

#9. Market Book Ratio
br <- ggplot(bank, aes(market_book_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-3000,5000) +
  scale_y_log10()
br

#10. Assets Growth
ag <- ggplot(bank, aes(x=assets_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,5) + 
  scale_y_log10()
ag

#11. Sales Growth
sg <- ggplot(bank, aes(x=sales_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,5) + 
  scale_y_log10()
sg

#12. Employee Growth
eg <- ggplot(bank, aes(x=employee_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,5) + 
  scale_y_log10()
eg

figure1 <- ggarrange(eps, liq, prof, 
                     prod, lr, at,
                     om, roe, br,
                     ag,sg,eg,
                     nrow = 4, ncol = 3)
figure1

bank_filtered <- bank %>%
  filter(between(eps, -50,50),
         between(liquidity, -100, ),
         between(profitability, -50, 50),
         between(productivity, -50, 50),
         between(leverage_ratio, -2, 2),
         between(asset_turnover, 0, 5),
         between(operational_margin, -2, 2),
         between(return_on_equity, -2, 2),
         between(market_book_ratio, -1000, 5000),
         between(assets_growth, -2, 2),
         between(sales_growth, -2, 2),
         between(employee_growth, -2, 2))

bank_filtered
 
bank_filtered %>%
  count(bk) %>%
  mutate(percent = n/sum(n)*100)

#check for missing values
colSums(is.na(bank))


