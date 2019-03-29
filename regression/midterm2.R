# Sam Tenney, Glen Thacker, Justin Woffinden
# Midterm 2

# Read in the data
detroit <- read.table(text = "
Year,TVaud,Tigers,AtBreak,DNP,Bullpen
2004,235.6,2,0.483,1,10
2005,436.6,1,0.488,4,10
2006,290.9,3,0.670,7,10
2007,305.1,5,0.605,2,10
2008,223.3,1,0.500,4,11
2009,302.1,5,0.552,19,13
2010,260.8,3,0.558,10,16
2011,201.9,5,0.533,15,13
2012,296.2,3,0.512,6,9
2013,374.7,6,0.553,8,14
2014,279.9,4,0.582,17,12
2015,190.4,4,0.500,8,12
2016,122.1,1,0.517,11,14
2017,111.2,2,0.448,7,11
2018,87.3,1,0.418,9,14", header = TRUE, sep = ",")

# Calculate the means, standard deviations and correlation to TVAud
colMeans(detroit[-1])
apply(detroit[-1], 2, sd)
cor(detroit[c(-1,-2)], detroit$TVaud)

# Model: TVaud = beta0 + beta1 Tigers + beta2 AtBreak + beta3 DNP
#               + beta4 Bullpen + epsilon, epsilon ~ N(0, sigma2)
detroit.out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = detroit)

# Calculate the R-Studentized residual
detroit.Rstud <- rstudent(detroit.out)

# Confirm that 2005 has an R-studentized residual that exceeds the rule of thumb ( > 3)
subset(detroit, abs(detroit.Rstud) > 3)
detroit.Rstud

# Remove 2005 from the dataset
detroit2 <- subset(detroit, detroit$Year != 2005)
detroit2.out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = detroit2)
detroit2.Rstud <- rstudent(detroit2.out)

# Test for normality using Shapiro-Wilkes Test
shapiro.test(detroit2.Rstud)

# Run the diagnostics tests
# Leverage
detroit.leverage<-lm.influence(detroit2.out)$hat
subset(detroit,detroit.leverage>2*5/14)

# Cook's Distance
detroit.cd<-cooks.distance(detroit2.out)
subset(detroit,detroit.cd>4/(14-5))

# Get parameter estimates and standard errors
summary(detroit2.out)
confint(detroit2.out)

# Graphic
plot(detroit2$Tigers, detroit2$TVaud, 
     xlab = "Tigers in All-Star Game",
     ylab = "Detroit TV Audience",
     main = "Detroit All-Stars and TV Audience")
abline(lm(TVaud ~ Tigers, data = detroit), col = "black")
     
plot(detroit2$Bullpen, detroit2$TVaud, 
     xlab = "Tiger Bullpen Pitchers in All-Star Game",
     ylab = "Detroit TV Audience",
     main = "Detroit Bullpen and TV Audience")
abline(lm(TVaud ~ Bullpen, data = detroit), col = "black")
     
detroit.reduced <- lm(TVaud ~ Tigers + AtBreak, data = detroit2)
anova(detroit.reduced, detroit2.out)

# predict successful Tigers
success.tigers <- predict(detroit2.out,
        newdata = data.frame(Tigers = 5,
                             AtBreak = 0.600,
                             DNP = 7,
                             Bullpen = 11),
        type = "response", se.fit = TRUE)

success.L <- success.tigers$fit - 1.96 * success.tigers$se.fit
success.U <- success.tigers$fit + 1.96 * success.tigers$se.fit

success.L
success.U

# Predict Tanking Tigers
tank.tigers <- predict(detroit2.out,
        newdata = data.frame(Tigers = 1,
                             AtBreak = 0.400,
                             DNP = 7,
                             Bullpen = 11),
        type = "response", se.fit = TRUE)

tank.tigers
tank.L <- tank.tigers$fit - 1.96 * tank.tigers$se.fit
tank.U <- tank.tigers$fit + 1.96 * tank.tigers$se.fit

tank.L
tank.U
