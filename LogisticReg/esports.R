# Sam Tenney
# eSports

library(httr)
library(jsonlite)

# Data
get1 <- GET("https://s3-us-west-1.amazonaws.com/riot-developer-portal/seed-data/matches10.json")
alljson1 <- fromJSON(content(get1, "text", encoding = "UTF-8"))

# get the 12th chunk
for (i in 1:100) {
  current_chunk <- data.frame(Win = ifelse(alljson1$matches$participants[[i]]$stats$win == TRUE, 1, 0),
                         Kills = alljson1$matches$participants[[i]]$stats$kills,
                         Deaths = alljson1$matches$participants[[i]]$stats$deaths,
                         Assists = alljson1$matches$participants[[i]]$stats$assists,
                         GoldEarned = alljson1$matches$participants[[i]]$stats$goldEarned,
                         LongestTime = alljson1$matches$participants[[i]]$stats$longestTimeSpentLiving,
                         LargestMultiKill = alljson1$matches$participants[[i]]$stats$largestMultiKill)
  if (i == 1) {
    legendsData <- current_chunk
  }
  else {
    legendsData <- rbind(legendsData, current_chunk)
  }

}

str(legendsData)

# EDA
par(mfrow=c(3,2))
boxplot(Kills ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Kills",
        ylab = "Kills")
boxplot(Deaths ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Deaths",
        ylab = "Deaths")
boxplot(Assists ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Assists",
        ylab = "Assists")
boxplot(GoldEarned ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Gold Earned",
        ylab = "Gold Earned")
boxplot(LongestTime ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Longest Time",
        ylab = "Longest Time")
boxplot(LargestMultiKill ~ Win, data = legendsData, names = c("Loss", "Win"),
        main = "Wins to Largest Multi Kill",
        ylab = "Largest Multi Kill")
par(mfrow=c(1,1))

# Look for anything unusual
summary(legendsData)
plot(~Kills+Deaths+Assists+LargestMultiKill+GoldEarned+LongestTime, data = legendsData)
plot(Kills ~ Deaths, data = legendsData)
plot(jitter(Kills) ~ jitter(Deaths), data = legendsData)
plot(jitter(Kills) ~ jitter(Assists), data = legendsData)
plot(jitter(Deaths) ~ jitter(Assists), data = legendsData)

# Remove obs 175, 322, 374, 526, 792 and any obs with Kills >= 20 & Deaths >= 15
legendsData <- legendsData[-c(175,322,374,526,792),]
legendsData <- subset(legendsData, Kills < 20 & Deaths < 15)
dim(legendsData)

# Analysis
# Response variable: Win = 1 if win, = 0 if lost
# Explanatory variables: 
#   Offense: Kills, GoldEarned
#   Errors: Deaths
#   Team Play: Assists
#   Risk/Reward: LongestTime
#   Hot Hand: LargestMultiKill  

# Create train and test
set.seed(58)
train_ind <- sample(989,700)

legends_train <- legendsData[train_ind,]
legends_test <- legendsData[-train_ind,]

# Model:
# log( P(win)/P(loss) ) = beta0 + beta1 Kills + beta2 GoldEarned + beta3 Deaths + beta4 Assists
#                         + beta5 LongestTime + beta6 LargestMultiKill

legends_out <- glm(Win ~ Kills + GoldEarned + Deaths + Assists + LongestTime + LargestMultiKill, data = legends_train, family = "binomial")

# Table of coefficients and std errors
summary(legends_out)

# For a one unit increase in Kills, we estimate a 0.0624 increase in the log odds ratio of wins,
# holding all else constant.
# a more interpretable version
exp( coef(legends_out)[-1] )

# Kills have no statistically significant effect on Wins (p-value = 0.1962)
# For a one additional Kill holding all else constant, we estimate a 6.4% increase
# (95% CI: -3%, 17%) in odds of winning

# Deaths have a statistically significant effect on Wins (p-value < 0.001)
# For each additional Death holding all else constant, we estimate a 53% decrease 
# (95% CI: 47%, 60%) in odds of winning (100-46.6)

# 95% CI on exp(beta_j)
exp( confint(legends_out)[-1,] )

# Graphics of effects

# Partial log-odds
x_star <- seq(0,10, length = 100)
plot(x_star, coef(legends_out)[2]* x_star, type = "l",
     xlab = "Kills", ylab = "Partial Logit(Win)") # Partial log of Odds Ratio of Wins
# Weakness, have to describe what logit transformation is

# From a probability perspective (by setting explanatory variables to median)
x_star <- data.frame(Kills = seq(0,10, length = 100), GoldEarned = 11000, Deaths = 5, Assists = 7, LongestTime = 600, LargestMultiKill = 1)
plot(x_star$Kills, predict(legends_out, newdata = x_star,type = "response"), 
     type = "l",
     xlab = "Kills", ylab = "Probability of Winning", ylim = c(0,1))

# From probability perspective when changing GoldEarned
x_star <- data.frame(GoldEarned = seq(5000, 17500, length = 100), 
                     Kills = 5, 
                     Deaths = 5, 
                     Assists = 7, 
                     LongestTime = 600, 
                     LargestMultiKill = 1)
plot(x_star$GoldEarned, predict(legends_out, newdata = x_star,type = "response"), 
     type = "l",
     xlab = "GoldEarned", ylab = "Probability of Winning", ylim = c(0,1))

# Summarize Effect on Winning
summary(legends_out)

# Does playing aggressively have a statistically significant effect?
# Ho: Kills, Assists, LargestMultiKill have no effect
# Reduced model assumes the null hypothesis is true
legends_reduced <- glm(Win ~ GoldEarned + Deaths + LongestTime, 
                       data = legends_train, 
                       family = "binomial")

# anova(reduced model, full model, test = "Chisq")
anova(legends_reduced, legends_out, test = "Chisq")

# There is a statistically significant effect for being aggressive (p-value < 0.001)

# Predict P(Win) for a player with Faker-like skills
predict(legends_out, 
        newdata = data.frame(Kills = 2,
                              GoldEarned = 15000,
                              Deaths = 3, 
                              Assists = 8,
                              LongestTime = 600,
                              LargestMultiKill = 2), 
        type = "response")

# 95% CI on the P(Win)
# Logit
Faker_logit <- predict(legends_out, 
        newdata = data.frame(Kills = 2,
                              GoldEarned = 15000,
                              Deaths = 3, 
                              Assists = 8,
                              LongestTime = 600,
                              LargestMultiKill = 2), 
        type = "link", se.fit = TRUE)
Faker_logit

#95% CI on logit
logit_L <- Faker_logit$fit - 1.96 * Faker_logit$se.fit
logit_U <- Faker_logit$fit + 1.96 * Faker_logit$se.fit

# 95% CI on P(Win)
Faker_phat_L <- exp(logit_L) / (1 + exp(logit_L))
Faker_phat_U <- exp(logit_U) / (1 + exp(logit_U))

# ROC curve (Receiver Operator Characteristic Curve)
library(ROCR)
train_pred <- prediction(predict(legends_out, type = "response"), legends_train$Win)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf,xlab = '1-specificity', ylab = 'sensitivity', main = "ROC Curve")
abline(0, 1, col = "gray")

# Compute the AUC (Area Under Curve) for training and test (= 91.75%)
performance(train_pred, measure = "auc")
