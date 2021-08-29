library(dplyr)
library(readxl)
library(caret)
library(ggplot2)

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

