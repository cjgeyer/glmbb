
library(glmbb)

# Geyer, Likelihood inference in exponential families and directions
#     of recession.  Electronic Journal of Statistics, 3, 259--289.
# Section 2.2

x <- 1:30
y <- ifelse(x <= 12 | x >= 24, 0, 1)

eout <- efglm(y ~ x + I(x^2), family = "binomial")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula) %*% eout$gdor)
identical(sign(eta), as.numeric(y == 1) - as.numeric(y == 0))

# Geyer, Likelihood inference in exponential families and directions
#     of recession.  Electronic Journal of Statistics, 3, 259--289.
# Section 2.3
d <- read.table("catrec.txt", header = TRUE)

eout <- efglm(y ~ (.)^3, family = "poisson", data = d)
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula, data = d) %*% eout$gdor)
all(zapsmall(eta) <= 0)
d[zapsmall(eta) < 0, ]
# above should agree with Table 2 in Geyer (2009)
# and output in *.Rout.save does

# Geyer, Likelihood inference in exponential families and directions
#     of recession.  Electronic Journal of Statistics, 3, 259--289.
# Section 2.4
d <- read.table("sports.txt", header = TRUE)

eout <- efglm(cbind(wins, losses) ~ 0 + ., data = d, family = "binomial")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
# for once we look at delta rather than eta
delta <- eout$gdor
names(delta) <- colnames(model.matrix(eout$formula, data = d))
delta
# above should agree with Table 4 in Geyer (2009)
# and output in *.Rout.save does in the sense that
# delta and the vector tabulated in Geyer (2009) agree up to
# addition of a constant to each term, which does not matter
# because of the nature of Bradley-Terry models.

