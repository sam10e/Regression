# Tree Models: Speeding

# Data: Last year's Montgomery County Traffic Violations filtered to speeding citations
# source('http://grimshawville.byu.edu/TrafficStops2018.R')
str(speed.last)
dim(speed.last)

# Response Variable - Speed when given a citation (quantitative)

# Explanatory Variables:
# Accident, Belts, Personal.Injury, Property.Damage, Commercial.License, Commercial.Vehicle
# Alcohol, Work.Zone, Color, Contributed.To.Accident, Race, Gender, AutoYear, Month, Hour,
# Out.of.State

# I think Gender has the largest effect on predicting speeding as 5606 of the 8713 observations
# were male.  Men are more associated with wanting to drive fast, and they do tend to 
# drive faster.
sum(with(speed.last, Gender == "M"))

# EDA
mean(speed.last$speed)
sd(speed.last$speed)
boxplot(speed.last$speed)
min(speed.last$speed)
max(speed.last$speed)

# The average speed over the posted speed limit is 15 mph with a standard deviation of 7 mph
# The speeds are skewed to the right, with 50% of the speeds being somewhere between
# 10 - 20 mph over the posted speed limit.  The speeds over the speed limit go from 
# 1 mph - 64 mph.

# Quantitative Explanatory Variables:
# AutoYear, Month (although cyclical), Hour

# Categorical Explanatory variables: 
# Accident, Belts, Personal.Injury, Property.Damage, Commercial.License, Commercial.Vehicle
# Alcohol, Work.Zone, Color, Contributed.To.Accident, Race, Gender, 
# Out.of.State


# Analysis

# Create train and test
set.seed(12)
n_speed_last <- dim(speed.last)[1] # gets the number of rows in the dataset
train_rows <- sample(x = n_speed_last, 8000) # Gets 8000 random row numbers from 1-8713
speed_train <- speed.last[train_rows,] # Sets speed_train to the random row numbers from train_rows
speed_test <- speed.last[-train_rows,] # sets speed_test to the rest of the rows that weren't used

# Fit a Random Forest Model for the quantitative response variable
# and report the training and test RMSE
library(randomForest)

# randomForest(x = explanatory variable, y = response variable, xtest and ytest = test variables)
out_speed <- randomForest(x = speed_train[,-18], 
                          y = speed_train$speed, 
                          xtest = speed_test[, -18], 
                          ytest = speed_test$speed,
                          replace = TRUE, # bootstrapping
                          keep.forest = TRUE, # store trees for future prediction
                          ntree = 50, # number of trees
                          mtry = 5, # rule of thumb is p/3, where p is how many columns you have
                          nodesize = 25) # how complicated are the trees.

out_speed

# notice the results give MSE and we want RMSE
sqrt(48.71651) # 6.97
sqrt(61.93) # 7.87 - this is what we want, the Test set MSE

# Predict the speed I'd receive a ticket for the explanatory variables given
# in new.obs
predict(out_speed, newdata = new.obs)

# To understand a little about the model
round(importance(out_speed),0)
varImpPlot(out_speed)

# Report the 3 most important factors in the estimated RF model
# Hour, SubAgency, Color
