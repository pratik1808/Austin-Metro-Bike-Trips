library(ggplot2)
library(cowplot)
library(randomForest)
library(dplyr)

#Raw data split into about 75/25 training and test.
train = read.csv('Training.csv')
test = read.csv('Test.csv')

####################################################################################
##Transform membership type from 1 string categorical variable to 10 dummy variables.
##This drops % variation explained to 0 (actually negative) and significantly increases MSE.
##Unnecessary for a tree-based model to convert to dummy integers.

# train$Local365 = train$Membership.Type
# train$DayWalkUpPass = train$Membership.Type
# train$Local30 = train$Membership.Type
# train$Explorer = train$Membership.Type
# train$WalkUp = train$Membership.Type
# train$Local31 = train$Membership.Type
# train$Weekender = train$Membership.Type
# train$DayKiosk = train$Membership.Type
# train$UTStudent = train$Membership.Type
# train$Student = train$Membership.Type
# 
# train$Local365 = ifelse(train$Local365=='Local365',1,0)
# train$DayWalkUpPass = ifelse(train$Local365=='24 Hour Walk Up Pass',1,0)
# train$Local30 = ifelse(train$Local365=='Local30',1,0)
# train$Explorer = ifelse(train$Local365=='Explorer',1,0)
# train$WalkUp = ifelse(train$Local365=='Walk Up',1,0)
# train$Local31 = ifelse(train$Local365=='Local 31',1,0)
# train$Weekender = ifelse(train$Local365=='Weekender',1,0)
# train$DayKiosk = ifelse(train$Local365=='24-Hour Kiosk (Austin B-cycle)',1,0)
# train$UTStudent = ifelse(train$Local365=='U.T. Student Membership',1,0)
# train$Student = ifelse(train$Local365=='Student Membership',1,0)
# 
# train = train[-1]
# 
# test$Local365 = test$Membership.Type
# test$DayWalkUpPass = test$Membership.Type
# test$Local30 = test$Membership.Type
# test$Explorer = test$Membership.Type
# test$WalkUp = test$Membership.Type
# test$Local31 = test$Membership.Type
# test$Weekender = test$Membership.Type
# test$DayKiosk = test$Membership.Type
# test$UTStudent = test$Membership.Type
# test$Student = test$Membership.Type
# 
# test$Local365 = ifelse(test$Local365=='Local365',1,0)
# test$DayWalkUpPass = ifelse(test$Local365=='24 Hour Walk Up Pass',1,0)
# test$Local30 = ifelse(test$Local365=='Local30',1,0)
# test$Explorer = ifelse(test$Local365=='Explorer',1,0)
# test$WalkUp = ifelse(test$Local365=='Walk Up',1,0)
# test$Local31 = ifelse(test$Local365=='Local 31',1,0)
# test$Weekender = ifelse(test$Local365=='Weekender',1,0)
# test$DayKiosk = ifelse(test$Local365=='24-Hour Kiosk (Austin B-cycle)',1,0)
# test$UTStudent = ifelse(test$Local365=='U.T. Student Membership',1,0)
# test$Student = ifelse(test$Local365=='Student Membership',1,0)
# 
# test = test[-1]
####################################################################################


#####Bagging

#Bagging - Create random forest with all variables (m=p=19)
set.seed(42)
n = nrow(train)

#Create a bagging model with 200 trees and M = p = 19 (this takes a long time..)
print('Creating random forest of 200 trees..')
bag_200 = randomForest(TripCount ~., data = train, ntree=200, mtry=19)
print(bag_200)
plot(bag_200)
varImpPlot(bag_200)
#membership type is by-far the most important predictor of volume


#Bagging with validation set

#Create validation set separated into 2 sets: x inputs for prediction, y values for validation
pred = subset(test, select = -TripCount)
predY = as.double(test$TripCount)

#Bagging model will create based on the train data and then test the error on the test data.
print('Creating random forest of 150 trees..')
bag_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150, mtry=19)
print(bag_val_150)
plot(bag_val_150)
varImpPlot(bag_val_150)

####################################################################################

## Random Forests - Create random forest with m = p/3 = about 6
print('Creating random forest of 300 trees..')
rf_300 = randomForest(TripCount ~., data = train, ntree=300)
print(rf_300)
plot(rf_300)
varImpPlot(rf_300)


### Random Forest Validation
rf_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150)
print(rf_val_150)
plot(rf_val_150)
varImpPlot(rf_val_150)

####################################################################################

##### Appendix

### Bagging without year
#Because past years cannot occur again, it's not a valuable variable for tree-based models (imagine a cut where year < 2017.. waste of predictive power)
#Therefore, removing years is a better way to anonymize a month and use the predictive variables to estimate volume on the next matching observation.
#Year can be included in beta-based models, as an increase in year may affect volume and past years are helpful to predict that.
train = train[-5]
test = test[-5]

set.seed(7)

###Creating a bagging model without year, so m = 18 (the new p)
print('Creating random forest of 150 trees..')
bag2_150 = randomForest(TripCount ~., data = train, ntree=150, mtry=18)
print(bag2_150)
plot(bag2_150)
varImpPlot(bag2_150)

### Recreate validation set
#create validation set separated into 2 sets: x inputs for prediction, y values for validation
pred = subset(test, select = -TripCount)
predY = as.double(test$TripCount)

##Validate the bagging model
print('Creating random forest of 150 trees..')
bag2_val_150 = randomForest(TripCount ~., data = train, xtest = pred, ytest = predY, ntree=150, mtry=18)
print(bag2_val_150)
plot(bag2_val_150)
varImpPlot(bag2_val_150)


####NOTES
#This has a bit less explanatory power, but that's due to the fact that year was such a significant teller in the tree-based model.
#However, since we randomized data in a 'time series' instead of using the most recent year as a hold-back, validating on 'skipped' months within a year is almost like an out-of-bag sample.
#It's kind of cheating, since the patterns for that specific year are so telling.
#The goal is to predict future volume based on time of year, membership type, etc, so a less explanatory model while testing will likely be more explanatory when predicting since it is not overfit to historical years.