
library(glmbb)

# Agresti, Categorical Data Analysis, third edition, 2013,
# Sec 6.5.1, Example 1

x <- seq(10, 90, 10)
x <- x[x != 50]
y <- as.numeric(x > 50)

eout <- llmdr(y ~ x, family = "binomial")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula) %*% eout$gdor)
identical(sign(eta), as.numeric(y == 1) - as.numeric(y == 0))

# Agresti, Categorical Data Analysis, third edition, 2013,
# Sec 6.5.1, Example 2

x <- c(x, 50, 50)
y <- c(y, 0, 1)

eout <- llmdr(y ~ x, family = "binomial")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula) %*% eout$gdor)
identical(sign(zapsmall(eta)), as.numeric(x > 50) - as.numeric(x < 50))

# Agresti, Categorical Data Analysis, third edition, 2013,
# Sec 6.5.2, Table 6.11

center <- rep(1:5, each = 2)
center <- as.factor(center)
treatment <- rep(c("active_drug", "placebo"), times = 5)
success <- c(0, 0, 1, 0, 0, 0, 6, 2, 5, 2)
failure <- c(5, 9, 12, 10, 7, 5, 3, 6, 9, 12)
y <- cbind(success, failure)

foo <- data.frame(center = rep(center, times = 2),
    treatment = rep(treatment, times = 2), y = c(success, failure),
    outcome = rep(c("success", "failure"), each = length(failure)))
foo <- transform(foo,
    outcome = factor(outcome, levels = c("success", "failure")))
foo <- xtabs(y ~ center + treatment + outcome, foo)
names(dimnames(foo))
# X Y marginal
apply(foo, c(2, 3), sum)
# Y Z marginal
apply(foo, c(1, 3), sum)
# OK.  Checks with book.

eout <- llmdr(y ~ center + treatment, family = "binomial")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula) %*% eout$gdor)
identical(sign(zapsmall(eta)), - as.numeric(center %in% c(1, 3)))

