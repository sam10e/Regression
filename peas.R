# Galton inheritance

# Read in the data
peas <- read.table(text = "
Parent, Offspring
0.21,0.1726
0.20,0.1707
0.19,0.1637
0.18,0.1640
0.17,0.1613
0.16,0.1617
0.15,0.1598", header = TRUE, sep = ",")

# Check the data to make sure it was read in correctly
str(peas)

# EDA

# Compute the correlation coefficient for Parents and Offspring peas
cor(peas$Parent, peas$Offspring)

# We get a r = .925, so a very strong correlation

# Create a scatterplot to confirm the correlation coefficient
plot(peas, type = "b", main = "Galton's Pea Diameters")

# We see that the plot is pretty straight, as a result of
# the data being strong and positive correlated.

# Analysis

# Response Variable: Offspring pea diameter (inches)
# Explanatory Variable: Parent pea diameter (inches)

# Model:
# Offspring = beta0 + beta1 Parent + epsilon, epsilon~N(0, sigma^2)
out_peas <- lm(Offspring ~ Parent, data = peas)

# Create a table of estimates and standard errors (t-test)
summary(out_peas)

# ANOVA F-test
anova(out_peas)

# 95% Confidence Interval on beta1
confint(out_peas)

# Grpahic showing the uncertainty in estimating the model
library(ggplot2)
qplot(Parent,Offspring, data = peas,
      geom = "smooth", formula = y~x, method = "lm", se=TRUE,
      xlab = "Diameter of Parent Pea",
      ylab = "Diamter of Offspring Pea")

# 95% CI for E(Offspring | Parent = 0.20)
predict(out_peas, newdata = data.frame(Parent = 0.20), interval = "confidence")

# 95% PI for Parent = 0.18
predict(out_peas, newdata = data.frame(Parent = 0.18), interval = "prediction")

# Graphic of uncertainty with predictions
plot.df <- cbind(peas, predict(out_peas, interval = "prediction"))
ggplot(plot.df, aes(Parent, Offspring)) +
  xlab("Diameter of Parent Pea") +
  ylab("Diameter of Offspring Pea") +
  geom_point() +
  geom_line(aes(y = fit), color = "royalblue") +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

# report R^2
summary(out_peas)
# % of variation in Y explained by X
# Adjusted R^2 = 0.8265

