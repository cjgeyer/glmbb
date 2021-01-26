
library(glmbb)

# Agresti, Categorical Data Analysis, third edition, 2013,
# Table 4.5

age <- rep(c("<55", ">=55"), times = 2)
valve <- rep(c("aortic", "mitral"), each = 2)
deaths <- c(4, 7, 1, 9)
time.at.risk <- c(1259, 1417, 2082, 1647)

data.frame(age, valve, deaths, time = time.at.risk)

eout <- llmdr(deaths ~ age + valve, offset = log(time.at.risk),
    family = "poisson")
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# Note that fitted values agree with Table 4.6 in Agresti
round(eout$glm$fitted.values, 2)
