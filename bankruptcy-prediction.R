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
eps <- ggplot(bank, aes(x=EPS)) + 
  geom_histogram()+
  xlim(-15,15)
eps

Liq <- ggplot(bank, aes(x=Liquidity)) + 
  geom_histogram()+
  xlim(-2,2)
Liq

prod <- ggplot(bank, aes(x=Productivity)) + 
  geom_histogram()+
  xlim(-2,2)
prod
