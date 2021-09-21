library(dplyr)
library(readxl)
library(caret)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(mice)
library(tidyr)
library(devtools)
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

###### !!!!!!!!!!!!!!!!!!! USE THIS FOR MODEL TRAINING: trainBank_SMOTE <------ #########