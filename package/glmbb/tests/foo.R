
 library(glmbb)
 data(crabs)

 gout <- glm(satell ~ color * spine * width * weight, family = poisson,
     data = crabs)
 sum(! is.na(gout$coefficients))
 gout <- glm(satell ~ 1, family = poisson, data = crabs)
 sum(! is.na(gout$coefficients))

 gout <- glmbb(satell ~ color * spine * width * weight, data = crabs)

 fits <- ls(envir = gout$envir, pattern = "^sha1")
 length(fits)
 criteria <- Map(function(x) get(x, envir = gout$envir)$criterion, fits)
 formulae <- Map(function(x) get(x, envir = gout$envir)$formula, fits)
 names(criteria) <- NULL
 names(formulae) <- NULL
 criteria <- unlist(criteria)
 formulae <- lapply(formulae, tidy.formula.hierarchical)
 formulae <- sapply(formulae, deparse, width.cutoff = 500)
 fred <- data.frame(criteria, formulae)
 fred <- fred[order(criteria), ]
 opt <- options(width = 132)
 print(fred[1:20, ], right = FALSE)
 options(opt)

 # check criteria
 criteria.too <- Map(function(x) get(x, envir = gout$envir)$aic, fits)
 names(criteria.too) <- NULL
 criteria.too <- unlist(criteria.too)
 identical(criteria, criteria.too)

 gout <- glmbb(satell ~ color * spine * width * weight,
     family = poisson, data = crabs, criterion = "BIC", cutoff = 10)

 fits <- ls(envir = gout$envir, pattern = "^sha1")
 length(fits)
 criteria <- Map(function(x) get(x, envir = gout$envir)$criterion, fits)
 formulae <- Map(function(x) get(x, envir = gout$envir)$formula, fits)
 names(criteria) <- NULL
 names(formulae) <- NULL
 criteria <- unlist(criteria)
 formulae <- lapply(formulae, tidy.formula.hierarchical)
 formulae <- sapply(formulae, deparse, width.cutoff = 500)
 fred <- data.frame(criteria, formulae)
 fred <- fred[order(criteria), ]
 print(fred[fred$criteria <= min(fred$criteria) + gout$cutoff, ], right = FALSE)

 # check criteria
 criteria.too <- Map(function(x) BIC(get(x, envir = gout$envir)), fits)
 names(criteria.too) <- NULL
 criteria.too <- unlist(criteria.too)
 identical(criteria, criteria.too)

 # now AICc

 gout <- glmbb(satell ~ color * spine * width * weight,
     family = poisson, data = crabs, criterion = "AICc", cutoff = 5)
 fits <- ls(envir = gout$envir, pattern = "^sha1")
 criteria <- Map(function(x) get(x, envir = gout$envir)$criterion, fits)
 criteria.too <- Map(function(x) get(x, envir = gout$envir)$aic, fits)
 p.too <- Map(function(x)
     sum(! is.na(get(x, envir = gout$envir)$coefficients)), fits)
 n <- nrow(crabs)
 criteria.too <- Map(function(x, p) x + 2 * p * (p + 1) / (n - p - 1),
     criteria.too, p.too)
 all.equal(criteria, criteria.too)

