# Will you get a ticket or warning?

# Validate that data was read in correctly
# Data from source('http://grimshawville.byu.edu/TrafficStops2018.R')
str(ticket.last)
summary(ticket.last)
dim(ticket.last)

# What is the response variable? -- Ticket or no ticket (categorical)

# What are the explanatory variables?

# Which explanatory variable has the largest effect on predicting speed?
mean(ticket.last$Gender == "M")

# EDA

# TRUE = Ticket, FALSE = Warning
# Compute the frequencey table and proportion table for tickets given out and warnings
ticketTable <- table(ticket.last$Ticket)
addmargins(ticketTable)
prop.table(ticketTable)

# Anaysis

# Because it's classification, we will make a 50-50 dataset 
# (half goods(warnings, FALSE), and half bads(tickets, TRUE))
# all bads, SRS w/o replacement of goods

# Bads - Tickets
all_bad <- subset(ticket.last, Ticket == "TRUE")
n_bad <- dim(all_bad)[1]

# Goods - Warnings
all_good <- subset(ticket.last, Ticket == "FALSE")
n_good <- dim(all_good)[1]

set.seed(42)
rows_good <- sample(n_good, n_bad) # get number of bad rows sample of good rows because bad rows doesn't have as many as good rows
sample_good <- all_good[rows_good, ] # Use row numbers from the sample to get the random observations from those who received warnings.

# Create dataset
ticket_model <- rbind(all_bad, sample_good)
dim(ticket_model)
table(ticket_model$Ticket)

# Create train & test
n_ticket_model <- dim(ticket_model)[1]
train_rows <- sample(n_ticket_model, 150000)
ticket_train <- ticket_model[train_rows, ]
ticket_test <- ticket_model[-train_rows, ]

# Confirm they are similar
table(ticket_train$Ticket)
table(ticket_test$Ticket)

# Fit a Random Forest Model for the quantitative response variable
# and report the training and test RMSE
library(randomForest)

# Find column with response variable
names(ticket_train) # Ticket is column 17

# randomForest(x = explanatory variable, y = response variable, xtest and ytest = test variables)
out_ticket <- randomForest(x = ticket_train[,-17], 
                          y = ticket_train$Ticket,# [,17] 
                          xtest = ticket_test[, -17], 
                          ytest = ticket_test$Ticket,
                          replace = TRUE, # bootstrapping
                          keep.forest = TRUE, # store trees for future prediction
                          ntree = 50, # number of trees (generic is 50)
                          mtry = 5, # rule of thumb is p/3, where p is how many columns you have (16/3 ~ 5)
                          nodesize = 25) # how complicated are the trees.

out_ticket

# Demonstrate a new prediction
new.obs
predict(out_ticket, newdata=new.obs)
# A warning would be given for the new.obs data according to the prediction

# To understand a little about the model
round(importance(out_ticket),0)
varImpPlot(out_ticket)

# 3 most important variables
# Color of car, Hour of day, Auto year of car