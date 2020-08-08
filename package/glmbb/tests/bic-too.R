
     ## Dobson (1990) Page 93: Randomized Controlled Trial :
     ## ripped off from help page for glm
     counts <- c(18,17,15,20,10,20,25,13,12)
     outcome <- gl(3,1,9)
     treatment <- gl(3,3)
     glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
## IGNORE_RDIFF_BEGIN
     summary(glm.D93)
## IGNORE_RDIFF_END

 library(glmbb)

 foo <- glmbb(counts ~ outcome + treatment, family = poisson, crit = "BIC")
## IGNORE_RDIFF_BEGIN
 summary(foo)
## IGNORE_RDIFF_END

 bar <- glmbb(counts ~ outcome + treatment, family = poisson, crit = "BIC",
     BIC.opt = "sum")
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
 foo.n <- length(counts)

 e <- bar$envir
 e.names <- ls(envir = e, pattern = "^sha1")

 bar.criteria <- Map(function(x) get(x, envir = e)$criterion, e.names)
 bar.deviance <- Map(function(x) get(x, envir = e)$criterion.deviance, e.names)
 bar.penalty <- Map(function(x) get(x, envir = e)$criterion.penalty, e.names)
 bar.formulae <- Map(function(x) get(x, envir = e)$formula, e.names)
 bar.formulae <- vapply(bar.formulae, tidy.formula.hierarchical, character(1))
 bar.coefficients <- Map(function(x) get(x, envir = e)$coefficients, e.names)
 bar.p <- lapply(bar.coefficients, function(x) sum(! is.na(x)))
 bar.n <- sum(counts)

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

