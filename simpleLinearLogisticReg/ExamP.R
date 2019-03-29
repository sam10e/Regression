# Sam Tenney
# Exam P

# Data

# Read in the data
exam <- read.csv(text = '
Student,ExamP,GPA
2,No Pass,3.42
3,No Pass,3.93
4,No Pass,3.41
5,No Pass,3.75
6,Passed,3.18
7,No Pass,2.76
8,No Pass,3.68
9,No Pass,3.11
10,Passed,3.77
11,No Pass,3.06
12,No Pass,3.89
13,No Pass,3.28
14,Passed,3.82
15,No Pass,2.8
16,No Pass,3.45
17,No Pass,3.75
18,No Pass,3.7
19,No Pass,2.61
20,No Pass,3.17
21,Passed,3.93
22,No Pass,2.94
23,No Pass,2.49
24,No Pass,3.77
25,No Pass,2.7
26,Passed,3.4
27,No Pass,3.87
28,No Pass,2.55
29,Passed,3.81
30,No Pass,1.92
31,No Pass,2.79
32,No Pass,3.42
33,Passed,3.91
34,No Pass,3.94
35,Passed,3.6
36,Passed,3.89
37,No Pass,3.66
38,No Pass,3.02
39,Passed,3.87
40,No Pass,3.92
41,No Pass,3.27
42,Passed,3.32
43,Passed,3.52
44,No Pass,3.75
45,No Pass,3.87
46,No Pass,2.67
', header = TRUE)

str(exam)

# EDA

# Make a proportion table
examtable <- table(exam$GPA>=3.5, exam$ExamP)
prop.table(examtable, margin = 1)

# Make a boxplot
boxplot(GPA ~ ExamP, data = exam)
title(main = "GPA of Students Taking Exam P", xlab = "Exam P Results", ylab = "GPA")


# Analysis
# How is it different?
# Response Variable: Pass (1), No Pass (0)
exam$Pass <- ifelse(exam$ExamP == "Passed", 1, 0)

# Explanatory variable: GPA (Quantitative)
# Model: 
# log( P(Pass) / P(No Pass) )
#   = beta0 + beta1 GPA

exam_out <- glm(Pass ~ GPA, data = exam, family = "binomial")
summary(exam_out)

# beta1 = 2.256
#   For a one unit increase in GPA we estimate an increase of 2.256 in the log odds ratio

# Interpret GPA effect
exp(2.256)
# estimated increase in odds of passing Exam P by 9.54
# (almost 10 times) for increasing GPA by 1 (student increasing from B to A)

# Is GPA statistically significant?
# Ho: beta1 = 0
# z-test
summary(exam_out)
# LRT X^2 test
exam_reduced <- glm(Pass ~ +1, data = exam, family = "binomial")
anova(exam_reduced, exam_out, test = "Chisq")

# 95% CI on beta1
confint(exam_out)
# 95% on exp(beta1)
exp( confint(exam_out)[-1,] )
# Since 1 is not in the interval (because exp), then it is statistically significant

# Predict probability of passing for students with 3.25 and 3.85 GPA
predict(exam_out, newdata = data.frame(GPA=c(3.25, 3.85)), type = "response")

# Graphic of logistic regression model that uses GPA to model passing Exam P
# Graphic must include the data, estimated logistic regression model and 95% CI for P(Passed | GPA = x)
plot(Pass ~ GPA, data = exam, pch = 19,
     xlab = "GPA", ylab = "P(Pass Exam P | GPA)")
# Curve
xstar <- seq(2, 4, length = 100)
phat <- predict(exam_out, newdata = data.frame(GPA = xstar), type = "response")
lines(xstar, phat, col = "gray", lwd = 2)
# CI on curve
logit_hat <- predict(exam_out, newdata = data.frame(GPA = xstar), type = "link", se = TRUE)
logit_L <- logit_hat$fit - 1.96*logit_hat$se
logit_U <- logit_hat$fit + 1.96*logit_hat$se
phat_L <- exp(logit_L) / (1+exp(logit_L))
phat_U <- exp(logit_U) / (1+exp(logit_U))
lines(xstar, phat_L, col = "gray", lty = 3)
lines(xstar, phat_U, col = "gray", lty = 3)

# Log-Odds ratio
plot(xstar, logit_hat$fit, type = "l", col = "gray", lwd = 2)
lines(xstar, logit_L, col = "gray", lty = 3)
lines(xstar, logit_U, col = "gray", lty = 3)