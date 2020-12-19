
library(glmbb)

# Eck and Geyer, (submitted) Computationally efficient likelihood inference
#     in exponential families when the maximum likelihood estimator
#     does not exist.  https://arxiv.org/abs/1803.11240
#     Supplementary material http://hdl.handle.net/11299/197369

d <- read.table(gzfile("bigcategorical.txt.gz"), header = TRUE)

eout <- efglm(y ~ 0 + (.)^4, family = "poisson", data = d)
## IGNORE_RDIFF_BEGIN
summary(eout)
eout
## IGNORE_RDIFF_END

# check GDOR is correct
eta <- as.numeric(model.matrix(eout$formula, data = d) %*% eout$gdor)
# need really sloppy tolerance here
all(eta < 1e-5)
d[eta < -1e-5, ]
# above should agree R output in Appendix K of Eck and Geyer (submitted)
# and output in *.Rout.save does
sum(eta < -1e-5)

