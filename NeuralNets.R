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
library(psych)
library(neuralnet)

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
bank <- drop_na(bank)

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

predict_nn_class <- ifelse(predict_nn$net.result>0.5287, 1, 0)
predict_nn_class

t <- table(predict_nn_class, test$bk)
confusionMatrix(t)



#Logistic Regression Model model

model <- glm(bk ~., data = balanced_data, family = binomial)
summary(model)

predict_glm <- predict(model, test, type = "response")

predict_glm_class <- as.factor(ifelse(predict_glm > 0.4, 1,0))
confusionMatrix(predict_glm_class, reference = as.factor(test$bk))
                