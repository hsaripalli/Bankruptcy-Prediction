# ************************************************************************
# SVM.R -  Support Vector Machines (SVM) file
# 
# This file runs the SVM model on the data and produces associated plots
# ************************************************************************



# ********************************************************************
# Load the main bankruptcy-prediction.R file
# You must choose this main R file and then choose the Excel data file
# ********************************************************************
source(file.choose())

# Load required libraries
library(e1071)

# **********
# I'm having trouble with this model hanging. I think it takes a lot of processing time
# ***********





# svmModel <-svm(bk ~. -Obs, data = testBank, kernel = "linear", cost = 1, scale = FALSE)
# print(svmModel)

# svmModel <-svm(Species ~., data = iris, kernel = "polynomial", cost = 1, scale = FALSE)
# print(svmModel)


# cor(testBank)
# cor(dplyr::select(testBank, -Obs))
# pairs(dplyr::select(testBank, -Obs))

#testBank <- dplyr::select(testBank, -Obs)
