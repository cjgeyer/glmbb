
 library(glmbb)
 data(crabs)

 gout <- glmbb(satell ~ color * spine * width * weight,
     data = crabs, cutoff = Inf)

 fits <- ls(envir = gout$envir, pattern = "^sha1")
 fits

 form1 <- Map(function (x) get(x, envir = gout$envir)$formula, fits)
 form1

 form2 <- sapply(form1, tidy.formula.hierarchical)
 names(form2) <- NULL
 form2

 foo <- Vectorize(function(x) isHierarchical(as.formula(x)))
 foo(form2)
# data.frame(hierarchical = foo(form2), formula = form2,
#     stringsAsFactors = FALSE)

 foo <- Vectorize(function(x) isGraphical(as.formula(x)))
 foo(form2)
# data.frame(hierarchical = foo(form2), formula = form2,
#     stringsAsFactors = FALSE)

