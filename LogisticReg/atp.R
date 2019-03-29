# Sam Tenney
# ATP

# Data

# Choose a comparison case for all CATEGORICAL explanatory variables
# Gender - Generally, more people watch the men's tournaments
tennis$Gender <- factor(tennis$Gender)
tennis$Gender <- relevel(tennis$Gender, "W")

# Tournament - More people watch the Grand Slam tournaments, so we are more concerned with these.
# More and more are retiring earlier in Grand Slams
tennis$Tournament <- factor(tennis$Tournament)
tennis$Tournament <- relevel(tennis$Tournament, "GrandSlam")

# Surface - More matches are played on hard courts than the other types (Clay, Grass)
tennis$Surface <- factor(tennis$Surface)
tennis$Surface <- relevel(tennis$Surface, "Hard")

# Round - Article stated that more and more players are leaving in the first round.
tennis$Round <- factor(tennis$Round)
tennis$Round <- relevel(tennis$Gender, "1st Round")

# Best.of - Men play in the best of 5 most of the time, and they are longer matches, we can see if duration of the match is a reason
tennis$Best.of <- factor(tennis$Best.of)
tennis$Best.of <- relevel(tennis$Best.of, "5")

str(tennis)
# Create training set
set.seed(42)
n_atp_new <- dim(tennis)[1]
train_rows <- sample(n_atp_new, 12000)
tennis_train <- tennis[train_rows, ]

# Create test dataset of remaining players
tennis_test <- tennis[-train_rows, ]

dim(tennis_train)
dim(tennis_test)

# EDA
# Make a boxplot of DNF (horizontal) and each quantitative explanatory variable (vertical)
par(mfrow = c(1,2))
boxplot(WRank ~ DNF, data = tennis, main = "Winner's Rank")
boxplot(LRank ~ DNF, data = tennis, main = "Loser's Rank")
par(mfrow = c(1,1))

# Make contigency table of all the categorical explanatory variables and DNF
prop.table(table(tennis_train$Gender, tennis_train$DNF), margin = 1)
table(tennis_train$Gender, tennis_train$DNF)

prop.table(table(tennis_train$Tournament, tennis_train$DNF), margin = 1)
table(tennis_train$Tournament, tennis_train$DNF)

prop.table(table(tennis_train$Surface, tennis_train$DNF), margin = 1)
table(tennis_train$Surface, tennis_train$DNF)

prop.table(table(tennis_train$Round, tennis_train$DNF), margin = 1)
table(tennis_train$Round, tennis_train$DNF)

prop.table(table(tennis_train$Best.of, tennis_train$DNF), margin = 1)
table(tennis_train$Best.of, tennis_train$DNF)

# Analysis
# Response Variable: DNF = 1 if match ended with Loser retiring or walkover, = 0 if match was completed
# Explanatory Variable:
  # Qualitative: Gender, Tournament, Surface, Round, Best.of
  # Quantitative: WRank, LRank

# Model:
# Logistic Regression - 
# log( P(DNP) / P(not DNP) ) = beta0 + beta1 Men + beta2 Masters + beta3 Clay + beta4 Grass
#                             + beta5 2ndRound + beta6 3rdRound + beta7 4thRound + beta8 Quarterfinals
#                             + beta9 Semifinals + beta10 TheFinal + beta11 Best of 3 + beta12 WRank
#                             + beta13 LRank
tennis.out <- glm(DNF ~ Gender + Tournament + Surface + Round + Best.of + WRank + LRank,
                  data = tennis_train,
                  family = binomial)

coef(tennis.out)
# Transform back into more interpretable form
exp( coef(tennis.out)[-1] )
# Construct 95% CI to test significance
confint(tennis.out)
exp( confint(tennis.out)[-1,] )

# There is no statistically significant difference between woemen and men (confint has 0 in it)
# in log-odds: Men are more likely to DNF than women (because it's positive coefficient)
# odds-ratio:
# holding all else constant men have a 0.4% (95% CI: -31%, 47%) increase in the odds of DNF compared to women.

# Wald test to test significance
# There is no statistically significant difference between women and men (p-value = 0.98) 
# after adjusting for all other factors
summary(tennis.out)

# Likelihood Ratio to test significance (assume Ho is true: take out Gender)
tennis.reduced <- glm(DNF ~ Tournament + Surface + Round + Best.of + WRank + LRank,
                  data = tennis_train,
                  family = binomial)

anova(tennis.reduced, tennis.out, test = "Chisq")
# There is no statistically significant difference between women and men (p-value = 0.98) 
# after adjusting for all other factors


# There is a statistically significant difference in tournament type (p-value = 0.0083)

# log odds: Masters tournaments have a larger DNF than Grand slams (because positive)
# odds-ratio:
# Holding all else constant Masters Tournaments have a 69% (95% CI: 14%, 149%)
# increase in the odds of DNF compared to Grand Slams

# Graphic of effect
plot(c(0,1), c(0, 0.523),
     pch = 19, # makes dot darker
     cex = 3, # makes dot bigger
     xlim = c(-0.5, 1.5), ylim = c(-0.1, 0.7),
     ylab = "Partial log(DNF)",
     xlab = "",
     axes = FALSE)
axis(2)
axis(1, c(0,1), c("Grand Slam", "Masters"))
box()
# arrows function gets CI

# Test Ho: Tournament round has no effect on DNF
tennis.reduced <- glm(DNF ~ Gender + Tournament + Surface + Best.of + WRank + LRank,
                      data = tennis_train,
                      family = binomial)

anova(tennis.reduced, tennis.out, test = "Chisq")

# Test Ho: Surface has no effect on DNF
tennis.reduced <- glm(DNF ~ Gender + Tournament + Round + Best.of + WRank + LRank,
                      data = tennis_train,
                      family = binomial)

anova(tennis.reduced, tennis.out, test = "Chisq")

# For an increase of one in Winner's Rank (got worse), we estimate an increase in odds of DNF of 0.2% holding all else constant.

# Prediction and 95% CI
predict(tennis.out, 
        newdata = data.frame(Gender = "M",
                                         Tournament = "GrandSlam",
                                         Round = "1st Round",
                                         Surface = "Hard",
                                         Best.of = "5",
                                         WRank = 50,
                                         LRank = 500),
        type = "response")

logit.hat <- predict(tennis.out, 
        newdata = data.frame(Gender = "M",
                                         Tournament = "GrandSlam",
                                         Round = "1st Round",
                                         Surface = "Hard",
                                         Best.of = "5",
                                         WRank = 50,
                                         LRank = 500),
        type = "link", se.fit = TRUE)

logit.L <- logit.hat$fit - 1.96 * logit.hat$se
logit.U <- logit.hat$fit + 1.96 * logit.hat$se

phat.L <- exp(logit.L/(1 + exp(logit.L)))
phat.U <- exp(logit.U/(1 + exp(logit.U)))

# ROC curve
library(ROCR)
# Train
tennis.pred <- prediction(predict(tennis.out, type = "response"), tennis_train$DNF)
tennis.perf <- performance(tennis.pred, measure = "tpr", x.measure = "fpr")

# Test
tennis.test.pred <- prediction(predict(tennis.out, newdata = tennis_test, type = "response"), tennis_test$DNF)
tennis.test.perf <- performance(tennis.test.pred, measure = "tpr", x.measure = "fpr")


plot(tennis.perf,
     xlab = "1-specificity",
     ylab = "sensitivity",
     main = "ROC")
abline(0,1, col = "gray") # Reference line.  Since curve is above, it's okay at making predictions, but not great.
plot(tennis.test.perf, add = TRUE, col = "red")

# Calculate the AUC
performance(tennis.pred, measure="auc")
performance(tennis.test.pred, measure = "auc")
