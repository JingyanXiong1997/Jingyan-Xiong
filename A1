set.seed(1234)
X1 <- runif(n = 10000, min = 1, max = 3)
X2 <- rgamma(n = 10000, shape = 3, scale = 2)
X3 <- rbinom(n = 10000, size = 1, prob = 0.3)
e <- rnorm(n = 10000, mean = 2, sd = 1) 

# create Y
Y <- 0.5 + 1.2*X1 - 0.9*X2 + 0.1*X3 + e

# create ydum
ydum <- ifelse(Y>mean(Y), 1, 0)

# correlation between Y and X1 
cor(Y, X1)
cor(Y, X1) - 1.2

# coefficient
X <- cbind(rep(1,10000), X1, X2, X3)
beta <- solve(t(X)%*%X)%*%t(X)%*%Y
beta

# standard error of OLS
error <- Y - X%*%beta
stanard_error <- sqrt(t(error)%*%error/(10000-4))
stanard_error

# probit model
probit <- glm(ydum ~ X1+X2+X3, family = binomial(link = "probit"))
summary(profit)

# logit model
logit <- glm(ydum ~ X1+X2+X3, family = binomial(link = "logit"))
summary(logit)

# linear
linear<- lm(ydum ~ X1+X2+X3)
summary(logit)

# compare the above three models
## The absolute values of coefficients of the probit model is smaller than 
## the absolute values of coefficients of the logit model.
## The estimated coefficients of the linear model is very similar to 
## the estimated coefficients of the logit model.
##For the three models, their estimated coefficients are all significant.

# marginal effect of profit model and the standard error
library(mfx)
data <- as.data.frame(cbind(ydum, X1, X2, X3))
probitmfx(ydum ~X1+X2+X3, data=data, atmean = TRUE)

# marginal effect of logit model and the standard error
logitmfx(ydum ~ X1+X2+X3, data=data, atmean = TRUE)
