# ************************************************************************
# Altman.R -  Altman Z score File
# 
# Runs calculations associated with Altman's Z Score
# Altman Z-Score = 1.2X1 + 1.4X2 + 3.3X3 + 0.6X4 + 1.0X5
# 
# ************************************************************************



# ********************************************************************
# Load the main bankruptcy-prediction.R file
# You must choose this main R file and then choose the Excel data file
# ********************************************************************
source(file.choose())

library(pROC)
library(ROCR)

trainBank_SMOTE <-dplyr::select(trainBank_SMOTE, -Obs)
testBank <-dplyr::select(testBank, -Obs)

#* Altman Z-Score = 1.2X1 + 1.4X2 + 3.3X3 + 0.6X4 + 1.0X5
#* X1 = liquidity
#* X2 = profitability
#* X3 = productivity
#* X4 = ??? Not sure what X4 is right now but using leverage ratio for now
#* X5 = asset_turnover
#*
altman <- 1.2*(trainBank_SMOTE$liquidity) + 1.4*(trainBank_SMOTE$profitability) + 
          3.3*(trainBank_SMOTE$productivity) + 0.6*(trainBank_SMOTE$leverage_ratio) +
          1.0*(trainBank_SMOTE$asset_turnover)

#*
#*Perform prediction on the training data
#*

altmanTrainingPredict <- altman

for (val in 1:701)
{
  if (altman[val] > 2.4)
  {
    altmanTrainingPredict[val] <-0
  }
  else
  {
    altmanTrainingPredict[val] <-1
    
  }
}

altmanTrainingPredict

#* Altman Z-Score = 1.2X1 + 1.4X2 + 3.3X3 + 0.6X4 + 1.0X5
#* X1 = liquidity
#* X2 = profitability
#* X3 = productivity
#* X4 = ??? Not sure what X4 is right now but using leverage ratio for now
#* X5 = asset_turnover
#*
altmanTest <- 1.2*(testBank$liquidity) + 1.4*(testBank$profitability) + 
              3.3*(testBank$productivity) + 0.6*(testBank$leverage_ratio) +
              1.0*(testBank$asset_turnover)



#*
#*Perform prediction on the testing data
#*

altmanTestPredict <- altmanTest

for (val in 1:24362)
{
  if (altmanTest[val] > 2.4)
  {
    altmanTestPredict[val] <-0
  }
  else
  {
    altmanTestPredict[val] <-1
    
  }
}

altmanTestPredict

# Confusion matrix
confusionMatrix(as.factor(altmanTestPredict), as.factor(testBank$bk))

par(pty = "s")


# ROC Curve
pROC::roc(testBank$bk, altmanTestPredict, plot=TRUE, print.auc = TRUE)



# NB - TPR vs FPR Plot and AUC

nb_predict <- prediction(altmanTestPredict, testBank$bk)
perf_nb <- performance(nb_predict, "tpr", "fpr")
plot(perf_nb, colorize = TRUE)

auc_nb <- performance(nb_predict, "auc")
auc_nb2 <- as.numeric(auc_nb@y.values)
auc_nb2

par(pty = "m")



