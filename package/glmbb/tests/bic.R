
     ## example from Venables and Ripley (2002, pp. 190-2.)
     ## ripped off from help page for predict.glm
     ldose <- rep(0:5, 2)
     numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
     sex <- factor(rep(c("M", "F"), c(6, 6)))
     SF <- cbind(numdead, numalive = 20-numdead)
     budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
## IGNORE_RDIFF_BEGIN
     summary(budworm.lg)
## IGNORE_RDIFF_END

 library(glmbb)

 foo <- glmbb(SF ~ sex*ldose, family = binomial, crit = "BIC")
## IGNORE_RDIFF_BEGIN
 summary(foo)
## IGNORE_RDIFF_END

 bar <- glmbb(SF ~ sex*ldose, family = binomial, crit = "BIC", BIC.opt = "sum")
## IGNORE_RDIFF_BEGIN
 summary(bar)
## IGNORE_RDIFF_END

 e <- foo$envir
 e.names <- ls(envir = e, pattern = "^sha1")
 names(get(e.names[1], envir = e))

 foo.criteria <- Map(function(x) get(x, envir = e)$criterion, e.names)
 foo.deviance <- Map(function(x) get(x, envir = e)$criterion.deviance, e.names)
 foo.penalty <- Map(function(x) get(x, envir = e)$criterion.penalty, e.names)
 foo.formulae <- Map(function(x) get(x, envir = e)$formula, e.names)
 foo.formulae <- vapply(foo.formulae, tidy.formula.hierarchical, character(1))
 foo.coefficients <- Map(function(x) get(x, envir = e)$coefficients, e.names)
 foo.p <- lapply(foo.coefficients, function(x) sum(! is.na(x)))
 foo.n <- length(numdead)

 e <- bar$envir
 e.names <- ls(envir = e, pattern = "^sha1")

 bar.criteria <- Map(function(x) get(x, envir = e)$criterion, e.names)
 bar.deviance <- Map(function(x) get(x, envir = e)$criterion.deviance, e.names)
 bar.penalty <- Map(function(x) get(x, envir = e)$criterion.penalty, e.names)
 bar.formulae <- Map(function(x) get(x, envir = e)$formula, e.names)
 bar.formulae <- vapply(bar.formulae, tidy.formula.hierarchical, character(1))
 bar.coefficients <- Map(function(x) get(x, envir = e)$coefficients, e.names)
 bar.p <- lapply(bar.coefficients, function(x) sum(! is.na(x)))
 bar.n <- sum(SF)

 idx <- match(foo.formulae, bar.formulae)
 all(! is.na(idx))

 all.equal(foo.deviance, bar.deviance[idx], check.attrib = FALSE)

 foo.penalty <- as.vector(unlist(foo.penalty))
 bar.penalty <- as.vector(unlist(bar.penalty))
 foo.p <- as.vector(unlist(foo.p))
 bar.p <- as.vector(unlist(bar.p))

 all.equal(foo.penalty, log(foo.n) * foo.p)
 all.equal(bar.penalty, log(bar.n) * bar.p)

 foo.deviance <- as.vector(unlist(foo.deviance))
 bar.deviance <- as.vector(unlist(bar.deviance))
 foo.criteria <- as.vector(unlist(foo.criteria))
 bar.criteria <- as.vector(unlist(bar.criteria))

 all.equal(foo.criteria, foo.deviance + foo.penalty)
 all.equal(bar.criteria, bar.deviance + bar.penalty)

