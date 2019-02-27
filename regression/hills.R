# Sam Tenney
# Scottish Hill Races

# Create a dataset of the distance and hill climb of Scottish hill races
# using http://www.statsci.org/data/general/hills.txt
hills <- read.table(header = TRUE, sep = "\t", text = '
Race	Distance	Climb	Time
Greenmantle	2.5	650	16.083
Carnethy	6	2500	48.35
CraigDunain	6	900	33.65
BenRha	7.5	800	45.6
BenLomond	8	3070	62.267
Goatfell	8	2866	73.217
BensofJura	16	7500	204.617
Cairnpapple	6	800	36.367
Scolty	5	800	29.75
Traprain	6	650	39.75
LairigGhru	28	2100	192.667
Dollar	5	2000	43.05
Lomonds	9.5	2200	65
CairnTable	6	500	44.133
EildonTwo	4.5	1500	26.933
Cairngorm	10	3000	72.25
SevenHills	14	2200	98.417
KnockHill	3	350	78.65
BlackHill	4.5	1000	17.417
CreagBeag	5.5	600	32.567
KildconHill	3	300	15.95
MeallAnt-Suidhe	3.5	1500	27.9
HalfBenNevis	6	2200	47.633
CowHill	2	900	17.933
NBerwickLaw	3	600	18.683
CreagDubh	4	2000	26.217
Burnswark	6	800	34.433
LargoLaw	5	950	28.567
Criffel	6.5	1750	50.5
Acmony	5	500	20.95
BenNevis	10	4400	85.583
Knockfarrel	6	600	32.383
TwoBreweries	18	5200	170.25
Cockleroi	4.5	850	28.1
MoffatChase	20	5000	159.833')

tail(hills)

# EDA
# Create scatterplots of 'Time' and the explanatory variables using identify()
plot(Time ~ Distance, data = hills, main = "Scottish Hills Race Distance and Time")
#identify(hills$Distance, hills$Time, hills$Race, cex=0.75)

plot(Time ~ Climb, data = hills, main = "Scottish Hills Race Climb and Time")
#identify(hills$Climb, hills$Time, hills$Race, cex=0.75)

# Analysis

# Model: Time = beta0 + beta1 Distance + beta2 Climb + epsilon, epsilon~(N, sigma2)
# Regression Diagnostics - making sure the assumptions are met
# Validity of normality assumption -- look at shape and outliers

hills_out <- lm(Time ~ Distance+Climb, data = hills)

# Before doing any inference, look at the data diagnostics (assumptions)
# "Estimate" the epsilons to investigate properties of epsilon

# Residuals: Actual - Prediction --> yi - yihat (where i is the observation -- subscript)
# R - Studentized Residual (R stands for remove the observation, Studentized is when sigma isn't known so t-distribution is removed)
# (yi - y(i)hat) / (SE(i)(yi - yihat))
hills_Rstud <- rstudent(hills_out)

# What is the R-studentized residual for Cairn Hill race?
subset(hills_Rstud, hills$Race == "CairnTable")

hist(hills_Rstud)
# prettier plot
hist(hills_Rstud, freq=FALSE)
my_z <- seq(-3,3,length = 50)
lines(my_z, dnorm(my_z,0,1), col = "royalblue")
# prettier plot with smooth lines
plot(density(hills_Rstud))
my_z <- seq(-3,3,length = 50)
lines(my_z, dnorm(my_z,0,1), col = "royalblue")

# Normality test
# Ho: Normally distributed
# Ha: Not normally distributed
shapiro.test(hills_Rstud)
# Very small p-value, so reject that it's normal.

# We can see there are some outliers .. what races are they?
# Is it Kildcon Hill?
# Ho: Kildcon hill is not an outlier
# Ha: Kildcon Hill is an outlier
# If Ho is true, it's a t-distribution with df = (35-1) -3 = 31.  
# Subtract 3 because there are 3 parameters (beta0, beta1 Distance, beta2 Climb)
# Subtract 1 because n = 35, but studentizing takes out an observation
subset(hills_Rstud, hills$Race == "KildconHill")
# p-value
2* (1 - pt(0.2054782, 31))
# Kildcon Hill is not an outlier (pvalue= 0.8385)

# Use a 'rule of thumb' to find outliers (2 std. deviations away)
subset(hills, abs(hills_Rstud) > 2)

# Is KnockHill an outlier?
subset(hills_Rstud, hills$Race == "KnockHill")
2*(1-pt(7.610845, 31))
# Yes, Knockhill is an outlier (pvalue < .001)

# Bonferroni adjustment to significance level
0.05/35

# Recognize when there are multiple explanatory variables, influential observations can be masked
# Leverage - Weight an observation has in predicting itself
hills_leverage <- lm.influence(hills_out)$hat

# Compute the leverage for the Ben Nevis race
subset(hills_leverage, hills$Race == "BenNevis")

# Rule of Thumb: 2*(p+1)/n where p = 2 (climb and distance) and n is the number of observations
subset(hills ,hills_leverage > 2 * 3 / 35)

# Cook's Distance - Change in parameter estimate with and without observations
hills_cd <- cooks.distance(hills_out)

# Compute Cook's Distance for Moffat Chase Race
subset(hills_cd, hills$Race == "MoffatChase")

# Rule of Thumb: 4/(n-(p+1))
subset(hills, hills_cd > 4 / (35 - (2 + 1)))

# Moffat Chase (row 35)
# Moffat Chase is influential as there is a big gap from a large part of the data
# but it is a good influence
par(mfrow = c(1, 2))
# Distance plot
plot(hills$Distance, hills$Time)
points(hills$Distance[35], hills$Time[35], col = "red", pch = 19)

# Climb plot
plot(hills$Climb, hills$Time)
points(hills$Climb[35], hills$Time[35], col = "red", pch = 19)

par(mfrow = c(1, 1))

# Lairig Ghru (row 11) appears to be bad influential
# Appears to be a long, flat race compared to the others.  Filter this race out.
par(mfrow = c(1, 2))
# Distance plot
plot(hills$Distance, hills$Time)
points(hills$Distance[11], hills$Time[11], col = "red", pch = 19)

# Climb plot
plot(hills$Climb, hills$Time)
points(hills$Climb[11], hills$Time[11], col = "red", pch = 19)

par(mfrow = c(1, 1))

# Bens of Jura (row 7) is a bad influential.
# Appears to be a shorter distance, but a steeper climb.
par(mfrow = c(1, 2))
# Distance plot
plot(hills$Distance, hills$Time)
points(hills$Distance[7], hills$Time[7], col = "red", pch = 19)

# Climb plot
plot(hills$Climb, hills$Time)
points(hills$Climb[7], hills$Time[7], col = "red", pch = 19)

par(mfrow = c(1, 1))


# Analysis
# Filter out Knock Hill (recorded incorrectly)
# Lairig Ghru (long flat .. most races have distance and climb correlated)
# Bens of Jura (this is hard due to terrain... terrain is not in the model)

hills1 <- hills[-c(18,11,7),]

# model: Time = beta0 + beta1 Distance + beta2 Climb + epsilon, epsilon~N(0,sigma2)

hills1_out1 <- lm(Time ~ Distance + Climb, data = hills1)
summary(hills1_out1)

# impact of iltering observations on analysis:
# only use the model for short small climb races (distance < 20, climb < 6000)
# prediction follows where we believed from the plots
# p-values a bit biased towards significance
# 95% PI will be biased smaller 
