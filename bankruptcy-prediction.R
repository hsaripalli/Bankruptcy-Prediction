library(dplyr)
library(readxl)
library(caret)

#import data
bank <- read_excel(file.choose())

#examine data
str(bank)
summary(bank)
head(bank)

