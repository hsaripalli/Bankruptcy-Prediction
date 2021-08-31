library(dplyr)
library(readxl)
library(caret)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(mice)

theme_set(theme_bw())

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

#correlation plot
corrplot(cor(bank, use = "complete.obs"), method = "number")

#histograms
#1. EPS
eps <- ggplot(bank, aes(x=eps)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-15,15)
eps

#2. Liquidity
liq <- ggplot(bank, aes(x=liquidity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
liq

#3. profitability
prof <- ggplot(bank, aes(x=profitability)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
prof

#4. productivity
prod <- ggplot(bank, aes(x=productivity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
prod

#5. Leverage Ratio
lr <- ggplot(bank, aes(x=leverage_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
lr

#6. Asset Turnover
at <- ggplot(bank, aes(asset_turnover)) +   
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-0,5)
at

#7. Operational Margin
om <- ggplot(bank, aes(operational_margin)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
om

#8. Return on Equity
roe <- ggplot(bank, aes(return_on_equity)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
roe

#9. Market Book Ratio
br <- ggplot(bank, aes(market_book_ratio)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-1000,1000)
br

#10. Assets Growth
ag <- ggplot(bank, aes(x=assets_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
ag

#11. Sales Growth
sg <- ggplot(bank, aes(x=sales_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
sg

#12. Employee Growth
eg <- ggplot(bank, aes(x=employee_growth)) + 
  geom_histogram(fill="steel blue", color = "black") +
  xlim(-2,2)
eg

figure1 <- ggarrange(eps, liq, prof, 
                     prod, lr, at,
                     om, roe, br,
                     ag,sg,eg,
                     nrow = 4, ncol = 3)
figure1

#for loop for histograms (doesn't work, any ideas how to make it work ? ) 

for (i in 1:12){
  plot_i <- ggplot(bank, aes(x=bank[,i]))+
    geom_histogram()
  plot_i
}


#*******************************************************
# Delete missing row code section
# 
# Comment this section out if you do not want to do this
#*******************************************************


# Split data between test and train sets (this is where the Excel files seems to split the test/train observations)
trainBank<-bank[1:88630,]
testBank<-bank[88631:92872,]

# Delete all rows with missing values in training set
trainBank <-filter(trainBank, !is.na(trainBank$eps))
trainBank <-filter(trainBank, !is.na(trainBank$liquidity))
trainBank <-filter(trainBank, !is.na(trainBank$profitability))
trainBank <-filter(trainBank, !is.na(trainBank$productivity))
trainBank <-filter(trainBank, !is.na(trainBank$leverage_ratio))
trainBank <-filter(trainBank, !is.na(trainBank$asset_turnover))
trainBank <-filter(trainBank, !is.na(trainBank$operational_margin))
trainBank <-filter(trainBank, !is.na(trainBank$return_on_equity))
trainBank <-filter(trainBank, !is.na(trainBank$assets_growth))
trainBank <-filter(trainBank, !is.na(trainBank$sales_growth))
trainBank <-filter(trainBank, !is.na(trainBank$market_book_ratio))
trainBank <-filter(trainBank, !is.na(trainBank$employee_growth))


# Delete all rows with missing values in testing set
testBank <-filter(testBank, !is.na(testBank$eps))
testBank <-filter(testBank, !is.na(testBank$liquidity))
testBank <-filter(testBank, !is.na(testBank$profitability))
testBank <-filter(testBank, !is.na(testBank$productivity))
testBank <-filter(testBank, !is.na(testBank$leverage_ratio))
testBank <-filter(testBank, !is.na(testBank$asset_turnover))
testBank <-filter(testBank, !is.na(testBank$operational_margin))
testBank <-filter(testBank, !is.na(testBank$return_on_equity))
testBank <-filter(testBank, !is.na(testBank$assets_growth))
testBank <-filter(testBank, !is.na(testBank$sales_growth))
testBank <-filter(testBank, !is.na(testBank$market_book_ratio))
testBank <-filter(testBank, !is.na(testBank$employee_growth))

# This cleans the entire original data set (not split)
cleanedBank <-filter(bank, !is.na(bank$eps))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$liquidity))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$profitability))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$productivity))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$leverage_ratio))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$asset_turnover))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$operational_margin))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$return_on_equity))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$assets_growth))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$sales_growth))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$market_book_ratio))
cleanedBank <-filter(cleanedBank, !is.na(cleanedBank$employee_growth))

# Checks for missing data
md.pattern(cleanedBank)
md.pattern(trainBank)
md.pattern(testBank)

