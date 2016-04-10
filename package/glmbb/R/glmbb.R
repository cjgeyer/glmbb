glmbb <- function(big, little = ~ 1, family = poisson, data,
    criterion = c("AIC", "AICc", "BIC"), cutoff = 0, ...) {

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

    y <- model.response(mf)

    stopifnot(is.numeric(cutoff))
    stopifnot(is.finite(cutoff))
    stopifnot(length(cutoff) == 1)
    stopifnot(cutoff >= 0)

    terms.big <- terms(big)
    terms.little <- terms(little)

    # patch up terms.little
    lab.little <- attr(terms.little, "term.labels")
    lab.big <- attr(terms.big, "term.labels")
    if (! all(lab.little %in% lab.big))
        stop("variables in little not in big")

    e <- new.env(hash = TRUE, parent = emptyenv())

    fitter <- function(mt) {
        x <- model.matrix(mt, mf)
        xhash <- sha1(colnames(x))
        xkey <- paste("sha1", xhash, sep = ".")
        if (exists(xkey, e)) {
            o <- get(xkey, e)
        } else {
            o <- glm.fit(x, y, ...)
            assign(xkey, o, e)
        }
        o
    }

    n <- length(y)

    criterion <- function(o) {
        if (criterion == "AIC") return(AIC(o))
        if (criterion == "BIC") return(BIC(o))
        p <- sum(! is.na(coefficients(o)))
        AIC(o) + 2 * p * (p + 1) / (n - p - 1)
    }

    min.crit <- Inf

    return(list(data = mf, little = little, big = big,
        terms.big = terms.big, terms.little = terms.little,
        family = family, y = y))
}

