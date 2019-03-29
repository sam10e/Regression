# Contrasts

matX <- matrix(c(0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,0), nrow = 8, ncol = 4)
solve(t(matX)%*%matX)

matY <- matrix(c(10,8,9,10,6,5,7,7), nrow = 8, ncol = 1)
betahat <- solve(t(matX)%*%matX)%*%t(matX)%*%matY
full.sse <- t(matY - matX%*%betahat)%*%(matY - matX%*%betahat)
red.sse <- sum((matY - mean(matY))^2)

fullmod <- lm(matY ~ matX%*%betahat)
red.mod <- lm(matY ~ 1)
anova(red.mod, fullmod)
