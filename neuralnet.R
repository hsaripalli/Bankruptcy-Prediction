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
library(neuralnet)
library(psych)
library(ROCR)
library(pROC)

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

#Noramizlie data to [-1,1] scale

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
