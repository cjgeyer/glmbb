
library(Matrix)
library(CatDataAnalysis)
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

formula <- count ~ food * (lake + gender + size)^2
conditioning <- ~ lake * gender * size

modmat.formula <- sparse.model.matrix(formula, data = d)
modmat.conditioning <- sparse.model.matrix(conditioning, data = d)

modmat.qr <- qr(modmat.conditioning)
foo <- qr.resid(modmat.qr, modmat.formula)
bar <- apply(foo^2, 2, sum)
names(bar)[bar < 1e-6]
min(bar[bar >= 1e-6])
names(bar)[bar >= 1e-6]


