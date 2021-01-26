
library(CatDataAnalysis)
library(glmbb)

# Agresti, Categorical Data Analysis, third edition, 2013,
# Chapter 8, Data in Table 8.1

data(table_8.1)
d <- transform(table_8.1,
    lake = factor(lake,
        labels = c("Hancock", "Oklawaha", "Trafford", "George")),
    gender = factor(gender, labels = c("Male", "Female")),
    size = factor(size, labels = c("<=2.3", ">2.3")),
    food = factor(food,
        labels = c("Fish", "Invertebrate", "Reptile", "Bird", "Other")))

# models that give warnings are
#
# count ~ lake * gender * size + food * lake * (gender + size)
# count ~ lake * gender * size + food * (lake + gender + size)^2
#
# demo that

gout <- glm(count ~ lake * gender * size + food * lake * (gender + size),
    family = poisson, data = d)

gout <- glm(count ~ lake * gender * size + food * (lake + gender + size)^2,
    family = poisson, data = d)

# now for GDOR

eout <- llmdr(count ~ food * (lake + gender + size)^2, data = d,
    conditioning = ~ lake * gender * size, family = "multinomial")

## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
modmat.formula <- Matrix::sparse.model.matrix(eout$formula, data = d)
modmat.conditioning <- Matrix::sparse.model.matrix(eout$conditioning, data = d)
modmat <- cbind2(modmat.conditioning, modmat.formula)
modmat <- modmat[ , names(eout$gdor)]
eta <- as.numeric(modmat %*% eout$gdor)
# eta
all(eta < sqrt(.Machine$double.eps))
all(eta >= - sqrt(.Machine$double.eps) | d$count == 0)

# and the other formula

eout <- llmdr(count ~ food * lake * (gender + size), data = d,
    conditioning = ~ lake * gender * size, family = "multinomial")

## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
modmat.formula <- Matrix::sparse.model.matrix(eout$formula, data = d)
modmat.conditioning <- Matrix::sparse.model.matrix(eout$conditioning, data = d)
modmat <- cbind2(modmat.conditioning, modmat.formula)
modmat <- modmat[ , names(eout$gdor)]
eta <- as.numeric(modmat %*% eout$gdor)
# eta
all(eta < sqrt(.Machine$double.eps))
all(eta >= - sqrt(.Machine$double.eps) | d$count == 0)



