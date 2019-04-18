# General Linear Models: Constructing the X Matrix, Basis Function Expansions for Nonlinear Effects

library(MASS)

head(mcycle)

plot(accel ~ times, data = mcycle)

# approximate with a high order polynomial
out1 <- lm(accel ~ times + I(times^2) + I(times^3) + I(times^4) + I(times^5),
           data = mcycle, x = TRUE)

xstar <- seq(0,60, length = 100)
yhat1 <- predict(out1, newdata = data.frame(times = xstar))
lines(xstar, yhat1, col = "red", lwd = 3)

head(out1$x)

plot(~ out1$x[,2] + out1$x[,3], out1$x[,4], out1$x[,5])

# Orthonormal polynomial basis function expansion (get the identity matrix)
out2 <- lm(accel ~ poly(times, 5), data = mcycle)
plot(accel ~ times, data = mcycle)
xstar <- seq(0,60, length = 100)
yhat2 <- predict(out2, newdata = data.frame(times = xstar))
lines(xstar, yhat2, col = "red", lwd = 3)


# piece wise linear with hockey stick basis function expansion
# knots: 14, 21, 30, 40 (look at the actual data)
out3 <- lm(accel ~ times + I( (times >= 14)*(times - 14) ) 
                         + I( (times >= 21)*(times - 21) )
                         + I( (times >= 30)*(times - 30) )
                         + I( (times >= 40)*(times - 40) ), data = mcycle)

plot(accel ~ times, data = mcycle)
xstar <- seq(0,60, length = 100)
yhat3 <- predict(out3, newdata = data.frame(times = xstar))
lines(xstar, yhat3, col = "red", lwd = 3)
