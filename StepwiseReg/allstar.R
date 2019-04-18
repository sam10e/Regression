# Stepwise Regression 1, All Star Game
# Sam Tenney

# Get the data at grimshawville.byu.edu/MLBbostonASG.txt
allstar <- read.table(file = "http://grimshawville.byu.edu/MLBbostonASG.txt", header = TRUE, sep = "", quote = "\"")

# EDA - see pdf turned in

# Analysis
# Akaike Information Criterion (AIC)

# Stepwise selection

# Backward Elmination isn't possible because we have wide skinny data

# Forward selection
min.model <- lm(aud/10^3 ~ +1, data = allstar)
biggest.model <- formula (lm(aud/10^3 ~ ., data = allstar) )
allstar.out2 <- step(min.model, scope = biggest.model, direction = "forward", steps = 2)

summary(allstar.out2)

library(car)
crPlots(allstar.out2)

# Model with 5 variables
min.model <- lm(aud/10^3 ~ +1, data = allstar)
biggest.model <- formula (lm(aud/10^3 ~ ., data = allstar) )
allstar.out5 <- step(min.model, scope = biggest.model, direction = "forward", steps = 5)

summary(allstar.out5)

# How to determine if the model is overfit
# R^2 value, very close to 1 or -1.  Shouldn't be too close in the real world
# Do the effects make sense?  
crPlots(allstar.out5)
