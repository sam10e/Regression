# Sam Tenney
# MLB AL East

# Data
# Create a dataset with the number of wins, divisions in the AL, and run.diff
# from the 2018 MLB season
# Data is found at http://espn.go.com/mlb/standings/_/season/2018

library(XML)
URL <- htmlParse("http://espn.go.com/mlb/standings/_/season/2018")
AL2018 <- readHTMLTable(URL, 
                        which = 4,
                        header = FALSE, 
                        colClasses = c(rep("numeric",4), rep("character",2), rep("numeric", 3), rep("character",2))
                        )

# Clean up rows with NA and unneeded columns
AL2018 <- AL2018[c(-1,-7,-13), c(1:3, 7:9)]

# Create division variable and add to AL2018 and rename all of the columns
division <- c(rep("East", 5), rep("Central", 5), rep("West", 5))
AL2018 <- cbind(AL2018, division)
colnames(AL2018) <- c("Wins", "Losses", "PercentWon", "RunsScored", "RunsAgainst", "RunDiff", "Division")

str(AL2018)

# EDA
# Calculate summary statistics for Wins and RunDiff separated by division
aggregate(Wins~Division, data = AL2018, FUN = mean)
aggregate(RunDiff~Division, data = AL2018, FUN = mean)
aggregate(Wins~Division, data = AL2018, FUN = sd)
aggregate(RunDiff~Division, data = AL2018, FUN = sd)

# Create a plot of run differential and wins with different colors for each division
library(ggplot2)

ggplot(AL2018, aes(x = RunDiff, y = Wins, color = Division)) +
  geom_point() +
  ggtitle("American League 2018 Wins and Run Differential") +
  xlab("Team Run Differential")
  

# Analysis (Notes from class)
# What makes this application different?
# Response Variable: Wins (Quantitative)
# Explanatory Variables: RunDiff (quantitative), Division (Qualitative)
# Note: 
  # If Division is ignored, then this is an Simple Linear Regression problem.  
  # If RunDiff is ignored, then it's a BF[1].  Factor: Division (East, Central, West)
  # "Analysis of Covariance", what old people call it.  It's regression with qualitative variables included

# Model:
  # If ignore Division: Wins = beta0 + beta1 RunDiff + epsilon, epsilon~N(0,sigma2)

  # (Think STAT 230 with i and j where they are factor levels and replicates) 
  # If ignore RunDiff: Wins(i,j) = GrandMean + alpha(i, where i is treatment effect) + epsilon(i,j), i = 1, 2, 3 (Division levels), j = 1, 2, 3, 4, 5 (teams in divisions)

# Division is a factor
AL2018$Division <- factor(AL2018$Division)

# Make AL East factor number 1
AL2018$Division <- relevel(AL2018$Division, "East")
str(AL2018)

# Model
AL2018_out <- lm(Wins ~ Division + RunDiff, data = AL2018, y = TRUE, x = TRUE)

# Examine the explanatory variables
AL2018_out$x

summary(AL2018_out)

# Interpret regression coefficient for RunDiff:
# For each run scored or prevented we estimate a 0.10
# increase in the number of wins keeping
# teams in the same division constant.

# What makes the division explanatory variable hard to interpret?
# 3 levels (East, Central, West)
# 'division main effect'?
# Each division is being compared to East

# Model: Wins = beta0 + beta1 DivCentral + beta2 DivWest + beta3 RunDiff + epsilon, epsilon~N(0,sigma2)
# Three models:
  # If East, then Wins = beta0 + beta1*0 + beta2*0 + beta3 RunDiff + epsilon
  # = beta0 + beta3 RunDiff

  # If Central, then Wins = beta0 + beta1*1 + beta2*0 + beta3 RunDiff + epsilon
  # = (beta0 + beta1) + beta3 RunDiff (it's just a line with beta0 and beta1 being the intercept)
  # Win difference for those teams with the same RunDiff.  The -2.84 for divisionCentral is how many wins less a central division team
  # would have than an Eastdivision team if they had the same RunDiff.

  # If West, then Wins = beta0 + beta1*0 + beta2*1 + beta3 RunDiff + epsilon
  # = (beta0 + beta2) + beta3 RunDiff (it's just a line with beta0 and beta1 being the intercept)

# Graphic of the different models (showing the effect of the division)
ggplot(AL2018, aes(x = RunDiff, y = Wins, color = Division)) +
  geom_point() +
  geom_abline(intercept = 81.840678, slope = 0.099961, col = "red") +
  geom_abline(intercept = 81.840678 - 2.843916, slope = 0.099961, col = "green") +
  geom_abline(intercept = 81.840678 + 0.241295, slope = 0.099961, col = "blue") +
  ggtitle("American League 2018 Wins and Run Differential") +
  xlab("Team Run Differential")

# It apears from the graph that there is no statistically significant difference
# between divisions after adjusting for run differential.
  
#Ho: No division effect/difference OR Ho: beta1 = 0 and beta2 = 0

# ANOVA F test
# Compare two models:
  # Assume Ho is true
  # If Ho is false

# Assume Ho is true
AL2018_reduced <- lm(Wins ~ RunDiff, data = AL2018, y = TRUE, x = TRUE)
# anova(assuming Ho is true, If Ho is false)
anova(AL2018_reduced, AL2018_out)

# There is no statistically significant difference in divisions(pvalue = 0.6172)
# after adjusting for run differential.