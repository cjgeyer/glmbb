
 library(digest)
 library(glmbb)

 foo <- terms(~ Contact * Influence * Housing + Satisfaction)
 bar <- attr(foo, "term.labels")
 baz <- glmbb:::standardize.term.labels(bar)

 foo <- terms(~ Satisfaction + Contact * Influence * Housing)
 bar <- attr(foo, "term.labels")
 qux <- glmbb:::standardize.term.labels(bar)

 ! identical(baz, qux)
 identical(sort(baz), sort(qux))
