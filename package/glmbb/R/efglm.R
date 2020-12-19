
efglm <- function(formula, data,
    family = c("poisson", "binomial", "multinomial"),
    conditioning, tolerance = 1e-3, ...) {

    family <- match.arg(family)

    stopifnot(inherits(formula, "formula"))

    if (missing(data))
        data <- environment(formula)

    modmat <- sparse.model.matrix(formula, data)

    # for following, see R function glm and
    # https://developer.r-project.org/model-fitting-functions.html
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "offset"),
        names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    mt <- attr(mf, "terms")
    response <- model.response(mf, "numeric")

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
            stopifnot(response %in% 0:1)
            n <- rep(1, length(response))
        }
        if (is.matrix(response)) {
            stopifnot(is.numeric(response))
            stopifnot(ncol(response) == 2)
            stopifnot(response == round(response))
            stopifnot(response >= 0)
            n <- rowSums(response)
            response <- response[ , 1]
        }
        if (is.factor(response)) {
            n <- rep(1, length(response))
            response <- as.numeric(response != levels(response)[1])
        }
        tangent.direction <- as.numeric(response == 0) -
            as.numeric(response == n)
    } else {
        # multinomial or product multinomial
        stopifnot(is.vector(response))
        stopifnot(length(response) == nrow(modmat))
        stopifnot(is.numeric(response))
        stopifnot(is.finite(response))
        stopifnot(round(response) == response)
        stopifnot(response >= 0)
        tangent.direction <- as.numeric(response == 0)
        if (! missing(conditioning))
            modmat.cond <- sparse.model.matrix(~ conditioning, data)
        else
            modmat.cond <- sparse.model.matrix(~ 1, data)
        modmat.save <- modmat
        modmat <- cbind2(modmat.cond, modmat)
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
    foo <- list(formula = formula, family = family)
    foo <- if (missing(conditioning)) foo else
        c(foo, list(conditioning = conditioning))
    foo <- c(foo, if (any(is.boundary.lcm))
        list(is.lcm = TRUE, is.fixed.lcm = is.boundary.lcm, gdor = gdor) else
        list(is.lcm = FALSE))
    class(foo) <- "efglm"
    return(foo)
}

