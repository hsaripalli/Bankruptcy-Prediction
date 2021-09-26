# ************************************************************************
# ExploratoryPlots.R -  creates Exploratory data plots
# 
# I did this so plots would not generate every time I run the code
# ************************************************************************



# ********************************************************************
# Load the main bankruptcy-prediction.R file
# You must choose this main R file and then choose the Excel data file
# ********************************************************************
source(file.choose())


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

