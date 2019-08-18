
# Multiple Linear Regression
# Created by Deepak Pradeep Kumar

#Setting working directory

setwd("<<Set Your Folder here>>")

# Importing the dataset 
dataset = read.csv("50_Startups.csv")

#Encoding the Categorical variable
dataset$State = factor(dataset$State, 
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3))

# Splitting the dataset into trainingset and testset
library(caTools)
set.seed(101)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

# Feature Scaling is not needed as it will take care of Fit function that we use.
# Fitting Multiple Linear Regression to training set

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = trainingset)

summary(regressor)

#Removing Administration and Fitting the model
regressor1 = lm(formula = Profit ~ R.D.Spend  + Marketing.Spend + State,
               data = trainingset)

summary(regressor1)

#Removing State and Fitting the model
regressor2 = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = trainingset)

summary(regressor2)

#Removing Marketing Spend and also and Fitting the model
regressor3 = lm(formula = Profit ~ R.D.Spend,
               data = trainingset)

summary(regressor3)

# Backward Elimination using StepAIC method
# Use this method immediately after Fitting the model 
#install.packages('MASS')
library(MASS)
stepAIC(regressor)


# Predicting the TEST SET Results
y_pred = predict(regressor, newdata = testset)

