BosTV <- read.table(text = '
Year TVaud BOS BAL NYY TB TOR
2004 303.3 3 1 8 1 1
2005 467.1 6 3 3 1 2
2006 422.8 4 1 4 1 5
2007 391.6 5 1 3 1 1
2008 403.8 7 1 3 3 1
2009 373.8 6 1 3 5 2
2010 263.5 6 1 8 5 3
2011 226.8 5 1 8 3 2
2012 183.9 1 3 4 2 1
2013 208.4 3 5 2 2 4
2014 208.2 2 3 3 1 3
2015 168.8 1 4 3 2 3
2016 245.4 6 5 3 1 5
2017 160.0 2 1 5 2 2
2018 173.2 5 1 4 2 1', header = TRUE, sep = "")

BosTV.out <- lm(TVaud ~ BOS + BAL + NYY + TB + TOR, data = BosTV)
bos.reduced <- lm(TVaud ~ 1, data = BosTV)
anova(bos.reduced, BosTV.out)
summary(BosTV.out)

sum((BosTV$TVaud - mean(BosTV$TVaud))^2)

anova(BosTV.out)

matY <- matrix(BosTV$TVaud)
matY

matX <- matrix(c(BosTV$BOS,BosTV$BAL,BosTV$NYY,BosTV$TB,BosTV$TOR), nrow = 15, ncol = 5)
matX
   
betaHat <- solve(t(matX)%*%matX)%*%t(matX)%*%matY    

solve(t(matX)%*%matX)

SoxYanks <- lm(TVaud ~ BOS + BAL + NYY + TB + TOR, data = BosTV)

summary(SoxYanks)

n <- dim(BosTV)[1]
p <- dim(matX)[2] 

s2 <- (1/(n - p + 1))%*%t(matY - matX%*%betaHat)%*%(matY - matX%*%betaHat)
s2

t(matY - (matX%*%betaHat))%*%(matY - (matX%*%betaHat))

summary(SoxYanks)

bos.reduced2 <- lm(TVaud ~ BAL + NYY + TB + TOR, data = BosTV)
anova(bos.reduced2, BosTV.out)

bos.reduced3 <- lm(TVaud ~ BOS, data = BosTV)

anova(bos.reduced3, BosTV.out)
