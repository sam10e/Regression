# Sam Tenney, Wade Farr, Eric Fortney

# Response variable: Switcher ( = 1, switcher from calculus, = 0 persistor in calculus)
# Explanatory variable: PrevCalc (1 HS Calc Experience, 4 = College Calc Experience, 5 = No Prev Calc Experience)
#   StandardizedTest
#   Major (1 = STEM, 2=Engineering, 3=Pre-med, 4=non-STEM, 5=undecided)
#   Teacher
#   ClassPractice
#   Gender (1 = male, 2 = female)

# Read in the data
stem <- read.csv("http://grimshawville.byu.edu/STEMswitchers.csv", header = TRUE)
str(stem)

stem$Switcher <- as.factor(stem$Switcher)
stem$PrevCalc <- as.factor(stem$PrevCalc)
stem$Major <- as.factor(stem$Major)
stem$Gender <- as.factor(stem$Gender)

# Create a side-by-side boxplot with Switcher on horizontal axis and each quantitative explanatory variable on vertical
par(mfrow=c(1,3))
boxplot(StandardizedTest ~ Switcher, data = stem, names = c("Persistor", "Switcher"),
        main = "Standardized Test to Switcher", ylab = "Standardized Test Scores")
boxplot(Teacher ~ Switcher, data = stem, names = c("Persistor", "Switcher"),
        main = "Teacher Score to Switcher", ylab = "Teacher Scores")
boxplot(ClassPractice ~ Switcher, data = stem, names = c("Persistor", "Switcher"),
        main = "Class Practice to Switcher", ylab = "Class Practice Scores")
par(mfrow=c(1,1))

# Make train and test data sets
set.seed(42)
train.ind <- dim(stem)[1]
train.rows <- sample(train.ind, 2000)
stem.train <- stem[train.rows,]
stem.test <- stem[-train.rows,]

# Create a contingency table of Switcher and each qualitative variable
prop.table(table(stem.train$PrevCalc, stem.train$Switcher), margin = 1)
table(stem.train$PrevCalc, stem.train$Switcher)

prop.table(table(stem.train$Major, stem.train$Switcher), margin = 1)
table(stem.train$Major, stem.train$Switcher)

prop.table(table(stem.train$Gender, stem.train$Switcher), margin = 1)
table(stem.train$Gender, stem.train$Switcher)

# Fit the model
stem.out <- glm(Switcher ~ PrevCalc + StandardizedTest + Major + Teacher + ClassPractice + Gender, data = stem.train, family = "binomial")
summary(stem.out)


# Make it more interpretable
exp( coef(stem.out)[-1] )

# 95% CI
exp( confint(stem.out)[-1,])

# Test Ho: no difference in previous calculus experience
prevcalc.out <- glm(Switcher ~ StandardizedTest + Major + Teacher + ClassPractice + Gender, data = stem.train, family = "binomial")
anova(prevcalc.out, stem.out, test = "Chisq")

# Test Ho: no difference in major
major.out <- glm(Switcher ~ PrevCalc + StandardizedTest + Teacher + ClassPractice + Gender, data = stem.train, family = "binomial")
anova(major.out, stem.out, test = "Chisq")

# Construct the ROC curve
library(ROCR)
train.pred <- prediction(predict(stem.out, type = "response"), stem.train$Switcher)
train.perf <- performance(train.pred, measure = "tpr", x.measure = "fpr")

# Test
stem.test.pred <- prediction(predict(stem.out, newdata = stem.test, type = "response"), stem.test$Switcher)
stem.test.perf <- performance(stem.test.pred, measure = "tpr", x.measure = "fpr")

plot(train.perf,
     xlab = '1-specificity', 
     ylab = 'sensitivity', 
     main = "ROC Curve", 
     col = "red")
abline(0, 1, col = "gray")
plot(stem.test.perf, add = TRUE, col = "blue")
legend("bottomright", legend = c("Training Data", "Testing Data", "Baseline"), col = c("red", "blue", "gray"), lty = 1)

# Compute the AUC (Area Under Curve) for training and test (= 91.75%)
performance(train.pred, measure = "auc")
