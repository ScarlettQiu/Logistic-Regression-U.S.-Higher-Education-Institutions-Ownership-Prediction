# Logistic-Regression-U.S.-Higher-Education-Institutions-Ownership-Prediction

Introduction

This project aims to create a logistic regression model to predict the university’s ownership type in the U.S. (private or public). The data set used in this report is from U.S. News and World Report which contains a large number of U.S. higher education institutions with information like whether they are private or public, the number of applications received number of applications accepted, out-of-state tuition, books costs, faculty-student ratio, graduation rate, etc. The data set has 777 observations and 18 variables, and no missing values. Among the 18 variables, there is 1 categorical variable which is binary and 17 numerical variables.


This report has done Exploratory Data Analysis first to first explore any variables that would be suitable to be the predictors (independent variables) for predicting whether the university is private or public (the dependent variable is Private). Since the Private variable is binary, this report has adopted logistic regression to create the model. Through the diagnosing process, this report has found the best fit model. Moreover, it has used training set and test set to do the cross-validation to avoid the over-fitting problem and leveraged the confusing matrix and plot ROC and calculate the AUC to further check the accuracy of the model.
