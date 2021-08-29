library(dplyr)
library(readxl)
library(caret)
library(ggplot2)
library(corrplot)

theme_set(theme_bw())

#import data
bank <- read_excel(file.choose())

#examine data
str(bank)
summary(bank)
head(bank)

#check the skewness of data
bank %>%
  count(BK) %>%
  mutate(percent = n/sum(n)*100)

#check for missing values
colSums(is.na(bank))

#correlation plot
corrplot(cor(bank, use = "complete.obs"), method = "number")

#histograms
#EPS
eps <- ggplot(bank, aes(x=EPS)) + 
  geom_histogram()+
  xlim(-15,15)
eps

#Liquidity
Liq <- ggplot(bank, aes(x=Liquidity)) + 
  geom_histogram()+
  xlim(-2,2)
Liq

#productivity
prod <- ggplot(bank, aes(x=Productivity)) + 
  geom_histogram()+
  xlim(-2,2)
prod

#Leverage Ratio
lr <- ggplot(bank, aes(x=bank$`Leverage Ratio`)) + 
  geom_histogram()+
  xlim(-2,2)
lr

#Asset Turnover
at <- ggplot(bank, aes(x=bank$`Asset Turnover`)) + 
  geom_histogram()+
  xlim(-2,2)
at

#Operational Margin
om <- ggplot(bank, aes(x=bank$`Operational Margin`)) + 
  geom_histogram()+
  xlim(-2,2)
om

#for loop for histograms (doesn't work) 

for (i in 1:12){
  plot_i <- ggplot(bank, aes(x=bank[,i]))+
    geom_histogram()
  plot_i
}
#test
#TEST




