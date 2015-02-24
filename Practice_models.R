x <- rnorm(1000)
y <- plnorm(x)
y <- exp(-(((x-mean(x))^2)/(2*((sd(x))^2))))

plot(y~x)
i

## From Jon Borelli
a <- rnorm(100)
b <- rnorm(100)
mod1 <- lm(a~b,contrasts = )
summary(mod1)

hist(mod1$residuals)
AIC(mod1)

