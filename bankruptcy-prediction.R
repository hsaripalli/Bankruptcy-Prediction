library(dplyr)
library(readxl)
library(caret)

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
