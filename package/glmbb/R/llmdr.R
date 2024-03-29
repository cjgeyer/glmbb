
llmdr <- function(formula, family = c("poisson", "binomial", "multinomial"),
    data, offset, conditioning, tolerance = 1e-3, ...) {

    family <- match.arg(family)

    stopifnot(inherits(formula, "formula"))

    if (missing(data))
        data <- environment(formula)

    stopifnot(is.numeric(tolerance))
    stopifnot(length(tolerance) == 1)
    stopifnot(tolerance > sqrt(.Machine$double.eps) && tolerance < 1)

    modmat <- sparse.model.matrix(formula, data)

    # for following, see R function glm and
    # https://developer.r-project.org/model-fitting-functions.html
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    mt <- attr(mf, "terms")
    response <- model.response(mf, "numeric")
    offset <- model.offset(mf)

    # for following, see writeup gdor.Rnw and efglm.Rnw
    # https://github.com/cjgeyer/glmbb/blob/master/devel/gdor.Rnw
    # https://github.com/cjgeyer/glmbb/blob/master/devel/efglm.Rnw

    # check response
    if (family == "poisson") {
        # response must be nonnegative-integer-valued
        stopifnot(is.vector(response))
        stopifnot(length(response) == nrow(modmat))
        stopifnot(is.numeric(response))
        stopifnot(is.finite(response))
        stopifnot(round(response) == response)
        stopifnot(response >= 0)
        tangent.direction <- as.numeric(response == 0)
    } else if (family == "binomial") {
        # response must be a zero-or-one-valued vector or a factor,
        # where the first level denotes failure and all others success, or
        # a two-column matrix with the columns giving the numbers of successes
        # and failures, respectively
        stopifnot(is.vector(response) || is.factor(response) ||
            is.matrix(response))
        if (is.vector(response)) {
            stopifnot(is.numeric(response))
            stopifnot(length(response) == nrow(modmat))
            stopifnot(response %in% 0:1)
            n <- rep(1, length(response))
        }
        if (is.matrix(response)) {
            stopifnot(is.numeric(response))
            stopifnot(nrow(response) == nrow(modmat))
            stopifnot(ncol(response) == 2)
            stopifnot(response == round(response))
            stopifnot(response >= 0)
            n <- rowSums(response)
            response.save <- response
            response <- response[ , 1]
        }
        if (is.factor(response)) {
            stopifnot(length(response) == nrow(modmat))
            n <- rep(1, length(response))
            response <- as.numeric(response != levels(response)[1])
        }
        tangent.direction <- as.numeric(response == 0) -
            as.numeric(response == n)
    } else {
        # multinomial or product multinomial
        stopifnot(is.vector(response))
        stopifnot(is.numeric(response))
        stopifnot(is.finite(response))
        stopifnot(round(response) == response)
        stopifnot(response >= 0)
        tangent.direction <- as.numeric(response == 0)
        if (! missing(conditioning)) {
            stopifnot(inherits(conditioning, "formula"))
            modmat.conditioning <- sparse.model.matrix(conditioning, data)
            if (! all(modmat.conditioning@x == 1))
                stop("argument conditioning must involve only factor variables")
        } else {
            modmat.conditioning <- sparse.model.matrix(~ 1, data)
        }
        factor.conditioning <-
            apply(modmat.conditioning, 1, paste, collapse = ":")
        qr.conditioning <- qr(modmat.conditioning)
        foo <- qr.resid(qr.conditioning, modmat)
        bar <- apply(foo^2, 2, sum)
        modmat.formula <- modmat
        modmat <- cbind2(modmat.conditioning,
            modmat[ , bar > tolerance, drop = FALSE])
    }

    # check offset
    if (!is.null(offset)) {
        stopifnot(is.numeric(offset))
        stopifnot(is.finite(offset))
        stopifnot(length(offset) == length(response))
    }

    objgrd <- rbind(tangent.direction) %*% modmat
    objgrd <- as(objgrd, "numeric")

    lp <- initProbCLP()
    resizeCLP(lp, nrow(modmat), 0)
    setLogLevelCLP(lp, 0)
    foo <- rep(Inf, ncol(modmat))
    addColsCLP(lp, ncol(modmat), -foo, foo, objgrd,
        modmat@p, modmat@i, modmat@x)
    chgRowLowerCLP(lp, pmin(- tangent.direction, 0))
    chgRowUpperCLP(lp, pmax(- tangent.direction, 0))
    setObjDirCLP(lp, 1)

    gdor <- rep(0, ncol(modmat))
    is.boundary.lcm <- rep(FALSE, length(response))
    is.boundary.unknown <- tangent.direction != 0
    repeat {
        primalCLP(lp)
        stopifnot(getSolStatusCLP(lp) == 0)
        if (getObjValCLP(lp) > (- tolerance)) break
        delta <- getColPrimCLP(lp)
        gdor <- gdor + delta
        eta <- modmat %*% delta
        eta <- as(eta, "numeric")
        is.boundary.lcm <- is.boundary.lcm | abs(eta) > tolerance
        is.boundary.unknown <- is.boundary.unknown & (! is.boundary.lcm)
        if (! any(is.boundary.unknown)) break
        objgrd <- rbind(
            as.numeric(is.boundary.unknown) * tangent.direction) %*% modmat
        objgrd <- as(objgrd, "numeric")
        chgObjCoefsCLP(lp, objgrd)
    }

    names(gdor) <- colnames(modmat)

    # adjust is.boundary.lcm in multinomial or product multinomial case
    if (family == "multinomial") {
        # if all but one component of a multinomial is zero,
        # then that one component is also nonrandom, equal to multinomial
        # sample size
        foo <- split(! is.boundary.lcm, factor.conditioning)
        bar <- sapply(foo, sum)
        baz <- names(bar)[bar == 1]
        is.boundary.lcm[factor.conditioning %in% baz] <- TRUE
    }

    # if (any(is.boundary.lcm)) then MLE is in LCM rather than OM
    # if (all(is.boundary.lcm)) then LCM is trivial, concentrated at one point

    if (! all(is.boundary.lcm)) {
        # fit LCM
        x <- as.matrix(modmat[! is.boundary.lcm, , drop = FALSE])
        if (exists("response.save")) {
            response <- response.save
            y <- response[! is.boundary.lcm, , drop = FALSE]
        } else {
            y <- response[! is.boundary.lcm]
        }
        f <- if (family == "binomial") binomial() else poisson()
        if (is.null(offset)) {
            gout <- stats::glm.fit(x, y, family = f, ...)
        } else {
            offset <- offset[! is.boundary.lcm]
            gout <- stats::glm.fit(x, y, family = f, offset = offset, ...)
        }
    }

    foo <- list(formula = formula, family = family)
    if (! missing(conditioning))
        foo <- c(foo, list(conditioning = conditioning))
    if (any(is.boundary.lcm)) {
        foo <- c(foo, list(is.lcm = TRUE, is.fixed.lcm = is.boundary.lcm,
            gdor = gdor))
    } else {
        foo <- c(foo, list(is.lcm = FALSE))
    }
    if (! all(is.boundary.lcm))
        foo <- c(foo, list(glm = gout))
    class(foo) <- "llmdr"
    return(foo)
}

summary.llmdr <- function(object, ...) {

    stopifnot(inherits(object, "llmdr"))

    invisible(NULL)
}
