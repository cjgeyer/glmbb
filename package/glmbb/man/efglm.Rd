\name{efglm}
\title{Exponential Family Generalized Linear Models}
\alias{efglm}
\concept{regression}
\concept{logistic}
\concept{log-linear}
\concept{loglinear}
\concept{Poisson}
\concept{multinomial}
\concept{product multinomial}
\concept{Barndorff-Nielsen completion}
\concept{aggregate exponential family}
\concept{direction of recession}
\concept{direction of constancy}
\description{
  Fit discrete exponential family generalized linear models (logistic 
  regression or Poisson regression with log link or multinomial regression
  with canonical link), correctly, even when 
  the maximum likelihood estimate does not exist in the conventional sense.
  Log-linear models for categorical data analysis are also fit correctly
  by considering them as Poisson regression.
}
\usage{
efglm(formula, data, family = c("poisson", "binomial", "multinomial"), ...)
}
\arguments{
  \item{formula}{an object of class \code{"\link[stats]{formula}"}
    (or one that can be coerced to that class): a symbolic description of the
    model to be fitted.
    See \code{\link[stats]{glm}} and \code{\link[stats]{formula}}
    for description of the R formula mini-language.}

  \item{family}{a character string specifying the family, must be one of
    \code{"poisson"} (default) or \code{"binomial"} or \code{"multinomial"}.
    May be abbreviated.}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which this function is called.}

  \item{...}{additional arguments passed to R function \code{\link[stats]{glm}}
    which is used to fit the limiting conditional model.
}
\details{
  In an exponential family generalized linear model (GLM) the maximum
  likelihood estimate need not exist.  This function detects this
  situation and does the right thing in that case.  For the Poisson,
  binomial, and multinomial models fit by this function the MLE always
  exists in the Barndorff-Nielsen completion of the original model (OM),
  and is always the MLE in the limiting conditional model (LCM), which
  conditions the OM on some components of the response vector being
  equal to their observed values.

  An LCM can be thought of in two ways.  It is obtained by conditioning
  the OM as just described.  It is also obtained by taking limits in
  the OM as parameters go to infinity.  See Geyer (2009) for further
  description.

  This function detects whether the MLE is in the OM or in an LCM,
  determines which LCM (which components of the response vector
  are conditioned), and fits the MLE in the LCM.

  The function \code{\link{summary}} (i.e., \code{\link{summary.efglm}})
  can be used to obtain or print a summary of the results.
}

\value{
  \code{efglm} returns an object of class inheriting from \code{"efglm"}
  and from \code{"glm"}, which is a list containing the result of calling
  R function \code{\link[stats]{glm}} to fit the LCM and, additionally
  some or all of the following components:
 
  \item{is.lcm}{logical.  \code{TRUE} indicates that the MLE for the
      regression coefficients does not exist and the model fit is
      the LCM.  \code{FALSE} indicates that the MLE for the original
      model does exist and the object returned is the model fit is the
      original model.}
  \item{is.zero.lcm}{a logical vector that says which components of
      the response vector are conditioned to be equal to their observed
      values in the LCM.  Not present when component \code{is.lcm} is
      \code{FALSE}.}
  T\item{gdor}{a generalized direction of recession (for which see
      Geyer, 2009).  Not present when component \code{is.lcm} is
      \code{FALSE}.}
}
\seealso{
  \code{\link{summary.efglm}}, etc.
}
\references{
  Geyer, C. J. (2009)
  Likelihood inference in exponential families and directions of recession.
  \emph{Electronic Journal of Statistics}, \bold{3}, 259--289.

  Eck, D.~J. and Geyer, C.~J. (submitted)
  Computationally efficient likelihood inference
    in exponential families when the maximum likelihood estimator
    does not exist.
  \url{https://arxiv.org/abs/1803.11240}
}

\examples{
# example of section 2.4 in Geyer (2009)
# data(sports)
# out <- glmdr(cbind(wins, losses) ~ 0 + ., family = "binomial", data = sports)
# summary(out)
}
\keyword{models}
\keyword{regression}
