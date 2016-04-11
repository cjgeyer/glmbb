
 library(glmbb)
 data(crabs)

 gout <- glm(satell ~ color * spine * width * weight, family = poisson,
     data = crabs)

 sum(! is.na(gout$coefficients))
 gout <- glm(satell ~ 1, family = poisson, data = crabs)
 sum(! is.na(gout$coefficients))

 gout <- glmbb(satell ~ color * spine * width * weight,
     family = poisson, data = crabs)

 fits <- ls(envir = gout$envir, pattern = "^sha1")
 length(fits)
 criteria <- Map(function(x) get(x, envir = gout$envir)$criterion, fits)
 formulae <- Map(function(x) get(x, envir = gout$envir)$formula, fits)
 names(criteria) <- NULL
 names(formulae) <- NULL
 criteria <- unlist(criteria)
 criteria
 formulae <- lapply(formulae, tidy.formula.hierarchical)
 formulae <- sapply(formulae, deparse, width.cutoff = 500)
 formulae
 fred <- data.frame(criteria, formulae)
 fred <- fred[order(criteria), ]
 fred[1:20, ]

 gout <- glmbb(satell ~ color * spine * width * weight,
     family = poisson, data = crabs, criterion = "BIC")

