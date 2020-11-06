
efglm <- function(formula, data,
    family = c("poisson", "binomial", "multinomial"), ...) {

    family <- match.arg(family)

    stopifnot(is.formula(formula))

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

    # for following, see writeup gdor.Rnw in
    # https://github.com/cjgeyer/glmbb/blob/master/devel/gdor.Rnw

    # check response
    if (family == "poisson") {
        # response must be nonnegative-integer-valued
        stopifnot(is.vector(response))
        stopifnot(length(response) == nrow(modmat))
        stopifnot(is.numeric(response))
        stopifnot(is.finite(response))
        stopifnot(round(response) == response)
        stopifnot(response >= 0)
        is.boundary <- response == 0
    } else if (family == "binomial") {
        # response must be a zero-or-one-valued vector or a factor,
        # where the first level denotes failure and all others success, or
        # a two-column matrix with the columns giving the numbers of successes
        # and failures, respectively
        stopifnot(is.vector(response) || is.factor(response) ||
            is.matrix(response))
        if (is.vector(response))
    }


    objgrd <- rbind(as.numeric(is.zero)) %*% modmat
    objgrd <- as(objgrd, "numeric")

    lp <- initProbCLP()
    resizeCLP(lp, nrow(modmat), 0)
    setLogLevelCLP(lp, 0)
    foo <- rep(Inf, ncol(modmat))
    addColsCLP(lp, ncol(modmat), -foo, foo, objgrd,
        modmat@p, modmat@i, modmat@x)
    chgRowLowerCLP(lp, - as.numeric(is.zero))
    chgRowUpperCLP(lp, rep(0, nrow(modmat)))
    setObjDirCLP(lp, 1)

    save.delta <- NULL
    is.zero.lcm <- rep(FALSE, length(response))
    repeat {
        primalCLP(lp)
        stopifnot(getSolStatusCLP(lp) == 0)
        if (getObjValCLP(lp) > (- tolerance)) break
        delta <- getColPrimCLP(lp)
        save.delta <- cbind(save.delta, delta)
        eta <- modmat %*% delta
        eta <- as(eta, "numeric")
        is.zero.lcm <- is.zero.lcm | eta < (- tolerance)
        is.zero.unknown <- is.zero & (! is.zero.lcm)
        objgrd <- rbind(as.numeric(is.zero.unknown)) %*% modmat
        objgrd <- as(objgrd, "numeric")
        chgObjCoefsCLP(lp, objgrd)
    }
    if (is.null(save.delta)) return(list(is.lcm = FALSE,
        is.zero.lcm = is.zero.lcm, delta = save.delta,
        gdor = NULL)) else return(list(is.lcm = TRUE,
        is.zero.lcm = is.zero.lcm, delta = save.delta,
        gdor = rowSums(save.delta)))
}

