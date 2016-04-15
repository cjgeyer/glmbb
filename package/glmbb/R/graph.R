isGraphical <- function(formula) {

    stopifnot(inherits(formula, "formula"))
    if (! is.hierarchical(terms(formula)))
        stop("model is not hierarchical")

    mt <- terms(formula)
    mf <- attr(mt, "factors")
    mr <- attr(mt, "response")
    if (mr != 0)
        mf <- mf[- mr, ]
    g <- matrix(0, nrow(mf), nrow(mf))
    for (i in 1:nrow(mf))
        for (j in 1:nrow(mf))
            if (i != j)
                g[i, j] <- any(mf[i, ] * mf[j, ] == 1)

    e <- new.env(hash = TRUE, parent = emptyenv())

    BronKerbosch <- function(R, P, X) {
        stopifnot(R %in% 1:nrow(mf))
        stopifnot(P %in% 1:nrow(mf))
        stopifnot(X %in% 1:nrow(mf))
        if (length(P) == 0 && length(X) == 0) {
           varname <- paste(c("foo", R), collapse = ".")
           assign(varname, R, envir = e)
        }
        for (v in P) {
            nv <- which(g[v, ] == 1)
            BronKerbosch(union(R, v), intersect(P, nv), intersect(X, nv))
            P <- setdiff(P, v)
            X <- union(X, v)
        }
    }

    foo <- BronKerbosch(integer(0), 1:nrow(mf), integer(0))
    bar <- as.list(e)
    names(bar) <- NULL

    ok <- TRUE
    for (i in bar) {
        qux <- rep(0, nrow(mf))
        qux[i] <- 1
        ok <- ok & any(apply(mf, 2, function(x) all(x == qux)))
    }
    return(ok)
}

isHierarchical <- function(formula) {
    stopifnot(inherits(formula, "formula"))
    is.hierarchical(terms(formula))
}

