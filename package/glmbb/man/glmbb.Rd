\name{glmbb}
\alias{glmbb}
\title{All Hierarchical Models for GLM via Branch and Bound}
\usage{
glmbb(big, little = ~ 1, family = poisson, data,
    criterion = c("AIC", "AICc", "BIC"), cutoff = 0, \dots) 
}
\description{
  Find all hierarchical models of specified GLM with
  information criterion (AIC, BIC, or AICc) within specified cutoff
  of minimum value.
  Use branch and bound algorithm so we do not have to fit all
  models.
}
\arguments{
  \item{big}{an object of class \code{"\link{stats}{formula}"} specifying
    the largest model to be considered.  Model specified must be hierarchical.
    (See also \code{\link{stats}{glm}} and \code{\link{stats}{formula}} and
    \sQuote{Details} section below.)}

  \item{little}{a formula specifying the smallest model to be considered.
    The response may be omitted and if not omitted is ignored (the response
    is taken from \code{big}).  Default is \code{~ 1}. Model specified must
    be nested within the model specified by \code{big}.}

  \item{family}{a description of the error distribution and link
    function to be used in the model.  This can be a
    character string naming a family function, a family function or the
    result of a call to a family function.  (See \code{\link{stats}{family}}
    for details of family functions.)}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{stats}{as.data.frame}} to a data frame)
    containing the variables in the models.  If not found in \code{data}, the
    variables are taken from \code{environment(big)},
    typically the environment from which \code{glmbb} is called.}

  \item{criterion}{a character string specifying the information criterion,
    must be one of \code{"AIC"} (Akaike Information Criterion, the default),
    \code{"BIC"} (Bayes Information Criterion) or \code{"AICc"} (AIC corrected
    for sample size).}

  \item{cutoff}{a nonnegative real number. This function finds all
    hierarchical models that are submodels of \code{big} and supermodels of
    \code{little} with information criterion less than or equal to the
    cutoff plus the minimum information criterion over all these models.}

  \item{\dots}{additional named or unnamed arguments to be passed
    to \code{stats}{glm.fit}.}
}
\details{
  Typical value for \code{big} is something like \code{foo ~ bar * baz * qux}
  where \code{foo} is the response variable (or matrix when family is
  \code{\link{stats}{binomial}} or \code{\link{stats}{quasibinomial}},
  see \code{\link{stats}{glm}}) and \code{bar}, \code{baz}, and \code{qux}
  are all the predictors that are considered for inclusion in models.

  A model is hierarchical if it includes all lower-order interactions for each
  term.  This is automatically what formulas with all variables connected by
  stars (\code{*}) do, like the example above.
  But other specifications are possible.
  For example, \code{foo ~ (bar + baz + qux)^2} specifies the model with all
  main effects, and all two-way interactions, but no three-way interaction,
  and this is hierarchical.

  A model \eqn{m_1}{m1} is nested within a model \eqn{m_1}{m2} if all terms
  in \eqn{m_1}{m1} are also terms in \eqn{m_2}.  The default little model
  \code{~ 1} is nested within every model except those specified to have
  no intercept by \code{0 +} or some such (see \code{link{stats}{formula}}).

}
\value{
  A list containing the following components:
}
\references{
  Hand, D. J. (1981)
  Branch and bound in statistical data analysis.
  \emph{The Statistician}, \bold{30}, 1--13.
}
\seealso{
  \code{link{stats}{family}},
  \code{link{stats}{formula}},
  \code{link{stats}{glm}},
}
\examples{
2 + 2
}
\keyword{models}
\keyword{regression}
\concept{model selection}
\concept{AIC}
\concept{BIC}
