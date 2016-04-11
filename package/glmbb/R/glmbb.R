glmbb <- function(big, little = ~ 1, family = poisson, data,
    criterion = c("AIC", "AICc", "BIC"), cutoff = 0, trace = FALSE, ...) {

    criterion <- match.arg(criterion)

    stopifnot(inherits(big, "formula"))
    stopifnot(inherits(little, "formula"))

    # next bit cut-and-pasted from the R function glm
    if (is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family))
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }

    # make data frame containing all variables
    if (missing(data)) {
        mf <- lm(big, method = "model.frame")
    } else {
        mf <- lm(big, data = data, method = "model.frame")
    }

    stopifnot(is.numeric(cutoff))
    stopifnot(is.finite(cutoff))
    stopifnot(length(cutoff) == 1)
    stopifnot(cutoff >= 0)

    terms.big <- terms(big)
    terms.little <- terms(little)

    lab.little <- attr(terms.little, "term.labels")
    lab.big <- attr(terms.big, "term.labels")
    lab.little <- standardize.term.labels(lab.little)
    lab.big <- standardize.term.labels(lab.big)
    if (! all(lab.little %in% lab.big))
        stop("terms in little not in big")
    intercept.little <- attr(terms.little, "intercept")
    intercept.big <- attr(terms.big, "intercept")
    if (intercept.little != intercept.big)
        stop("big and little models must both have intercept or neither")
    if (! is.hierarchical(terms.big))
        stop("big not hierarchical")
    if (! is.hierarchical(terms.little))
        stop("little not hierarchical")

    stopifnot(is.logical(trace))
    stopifnot(length(trace) == 1)

    # make little have same response as big
    little.char <- as.character(little)
    big.char <- as.character(big)
    if (length(little.char) == 2) {
        little.char <- c(big.char[2], little.char)
    } else {
        little.char <- c(big.char[2], little.char[c(1, 3)])
    }
    little.char <- paste(little.char, collapse = " ")
    little <- as.formula(little.char)

    e <- new.env(hash = TRUE, parent = emptyenv())

    n <- nrow(mf)

    e$min.crit <- Inf

    fitter <- function(f) {
        mt <- terms(f) 
        tl <- attr(mt, "term.labels")
        stl <- standardize.term.labels(tl)
        if (length(stl) == 0) stl <- "1"
        xhash <- sha1(stl)
        xkey <- paste("sha1", xhash, sep = ".")
        if (exists(xkey, e)) {
            o <- get(xkey, e)
        } else {
            if (trace)
                cat("        fitting model", deparse(f), "\n", file = stderr())
            o <- glm(f, family = family, data = mf, ...)
            p <- sum(! is.na(o$coefficients))
            aic <- AIC(o)
            if (criterion == "AIC") {
                o$criterion <- aic
                o$criterion.deviance <- aic - 2 * p
                o$criterion.penalty <- 2 * p
            }
            if (criterion == "BIC") {
                o$criterion <- BIC(o)
                o$criterion.deviance <- aic - 2 * p
                o$criterion.penalty <- log(n) * p
            }
            if (criterion == "AICc") {
                o$criterion.deviance <- aic - 2 * p
                o$criterion.penalty <- 2 * p + 2 * p * (p + 1) / (n - p - 1)
                o$criterion.penalty <- o$criterion.deviance +
                    o$criterion.penalty
            }
            assign(xkey, o, e)
            if (e$min.crit > o$criterion) e$min.crit <- o$criterion
            if (trace) {
                cat("        model criterion =", o$criterion, "\n",
                    file = stderr())
                cat("        minimum so far =", e$min.crit, "\n",
                    file = stderr())
            }
        }
        return(o)
    }

    # we now assume everything has been checked
    doit <- function(little, big) {
        if (trace) {
            cat("doit called with args\n", file = stderr())
            cat("   ", deparse(little), "\n", file = stderr())
            cat("   ", deparse(big), "\n", file = stderr())
        }
        # evaluate all models between little and big
        # or prove that they are not worth evaluating (above the cutoff)
        ol <- fitter(little)
        ob <- fitter(big)
        lower.bound <- ob$criterion.deviance + ol$criterion.penalty
        if (trace)
            cat("    lower bound for subfamily =", lower.bound, "\n",
                file = stderr())
        if (lower.bound > e$min.crit + cutoff) {
            if (trace)
                cat("    lower bound greater than minimum + cutoff; prune\n",
                    file = stderr())
            return(NULL)
        }
        # otherwise find a term in big but not in little and split on it
        ll <- attr(terms(little), "term.labels")
        lb <- attr(terms(big), "term.labels")
        ll <- standardize.term.labels(ll)
        lb <- standardize.term.labels(lb)
        if (! all(ll %in% lb))
            stop("can't happen: little bigger than big in subfamily")
        if (all(lb %in% ll)) {
            if (trace)
                cat("    little == big; hit bottom\n", file = stderr())
            return(NULL)
        }
        the.term <- setdiff(lb, ll)[1]
        if (trace)
            cat("    splitting on term", the.term, "\n", file = stderr())
        little.plus <- update(little, paste("~ . +", the.term))
        the.term.re <- unlist(strsplit(the.term, split = ":"))
        the.term.re <- paste(the.term.re, collapse = ".*")
        outies <- grep(the.term.re, lb, value = TRUE)
        outies <- paste(outies, collapse = " - ")
        big.minus <- update(big, paste("~ . -", outies))
        if (trace) {
            stopifnot(is.hierarchical(terms(little.plus)))
            stopifnot(is.hierarchical(terms(big.minus)))
        }
        doit(little.plus, big)
        doit(little, big.minus)
        return(NULL)
    }

    doit(little, big)

    return(list(data = mf, little = little, big = big,
        envir = e, min.crit = e$min.crit, cutoff = cutoff))
}

is.hierarchical <- function(mt) {
    stopifnot(inherits(mt, "terms"))
    f <- attr(mt, "factors")
    if (length(f) == 0) return(TRUE)
    stopifnot(is.matrix(f))
    for (j in 1:ncol(f)) {
       x <- f[ , j]
       ii <- which(x == 1)
       if (length(ii) == 1) next
       for (i in ii) {
           xtry <- x
           xtry[i] <- 0
           foo <- any(apply(f, 2, function(x) all(x == xtry)))
           if (! foo) return(FALSE)
       }
    }
    return(TRUE)
}

standardize.term.labels <- function(foo) {
    stopifnot(is.character(foo))
    bar <- strsplit(foo, split = ":")
    baz <- lapply(bar, sort)
    sapply(baz, paste, collapse = ":")
}

