# Sam Tenney
# Indicators.txt

# Chapter 2, exercise 2 (a, b)

# Data found at http://gattonweb.uky.edu/sheather/book/docs/datasets/indicators.txt in indicators.txt file

# Create a dataset
indicators <- read.table(header = TRUE, sep = "", text = '
MetroArea	PriceChange	LoanPaymentsOverdue
Atlanta		1.2	4.55
Boston		-3.4	3.31
Chicago		-0.9	2.99
Dallas		0.8	4.26
Denver		-0.7	3.56
Detroit		-9.7	4.71
LasVegas	-6.1	4.9
LosAngeles	-4.8	3.05
MiamiFt.Lauderdale	-6.4	5.63
MinneapolisStPaul	-3.4	3.01
NewYork		-3.8	3.29
Phoenix		-7.3	3.26
Portland	3.8	1.93
SanDiego	-7.8	3.45
SanFrancisco	-4.1	2.29
Seattle		6.9	1.65
Tampa		-8.8	4.6
WashingtonDC	-7.2	3.14')

# Fit the model: PriceChange = beta0 + beta1 LoanPaymentsOverdue + Epsilon
indicators_out <- lm(PriceChange ~ LoanPaymentsOverdue, data = indicators)
summary(indicators_out)

# Part a
# Find 95% confidence interval for beta1
confint(indicators_out)

# (95% CI: -4.16, -0.33)
# Since zero is not in the confidence interval, and both numbers are 
# negative, this is evidence that there is a significant negative linear association.

# Part b
# Use the fitted regression model to estimate E(Y|X = 4). 
# Find a 95% confidence interval
predict(indicators_out, newdata = data.frame(LoanPaymentsOverdue = 4), interval = "confidence")

# Is 0% a feasible value for E(Y|X=4)?


