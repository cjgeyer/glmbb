\name{llmdr}
\title{Log-Linear Models Done Right}
\alias{llmdr}
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
  Fit log-linear models: logistic regression, Poisson regression with log link,
  multinomial response regression with canonical link.  When the maximum
  likelihood estimate does not exist in the conventional sense, find the
  maximum likelihood estimate in the Barndorff-Nielsen completion of the model.
}
\usage{
llmdr(formula, data, family = c("poisson", "binomial", "multinomial"),
    conditioning, tolerance = 1e-3, \dots) 
}
\arguments{
  \item{formula}{an object of class \code{"\link[stats]{formula}"},
    a symbolic description of the model to be fitted.
    See \code{\link[stats]{glm}} and \code{\link[stats]{formula}}
    for description of the R formula mini-language.}

  \item{family}{a character string specifying the family, must be one of
    \code{"poisson"} (default) or \code{"binomial"} or \code{"multinomial"}.
    May be abbreviated.}

  \item{data}{an optional data frame, list, or environment (or object
    coercible by \code{\link{as.data.frame}} to a data frame) containing
    the variables in the model.  If not found in \code{data}, the
    variables are taken from \code{environment(formula)},
    typically the environment from which this function is called.}

  \item{conditioning}{a factor in the sense that
     \code{as.factor(conditioning)} is used
     (if not a factor, any type that has an \code{\link{as.character}} method),
     which has the same length as the response and predictor variables.
     Ignored unless \code{family = "multinomial"}.
     May be missing, which indicates
     multinomial conditioning: the sampling model is Poisson conditioned
     on the sum of the response vector being equal to its observed value.
     When not missing, indicates product multinomial conditioning:
     the sampling model is Poisson conditioned on the sum of the response
     vector over each level of the factor being equal to its observed value,
     that is, if \code{y} is the response vector, then the elements of
     \code{sum(split(y, conditioning))} are conditioned on being equal to
     their observed values.}

  \item{tolerance}{a number below which nonnegative quantities are considered
     to be zero in determining whether the MLE exists in the original model
     or in its Barndorff-Nielsen completion.  The very liberal default
     works fine unless the model matrix is very ill-conditioned.
     Must be strictly less than 1.0.}

  \item{\dots}{additional arguments passed to R function
    \code{\link[stats]{glm.fit}} which is used to fit the limiting
    conditional model.  The most useful are perhaps \code{offset}
    and \code{control}.}
}
\details{
  In an exponential family the maximum
  likelihood estimate need not exist.  This function detects this
  situation and does the right thing in that case.  For the Poisson,
  binomial, and multinomial models fit by this function the MLE always
  exists in the Barndorff-Nielsen completion of the original model (OM),
  and is always the MLE in the limiting conditional model (LCM), which
  conditions the OM on some components of the response vector being
  equal to their observed values.

  An LCM can be thought of in two ways.  It is obtained by conditioning
  the OM as just described.  It is also obtained by taking limits in
  the OM as submodel canonical parameters (\dQuote{coefficients}) go
  to infinity.  See Geyer (2009) for further description.

  This function detects whether the MLE is in the OM or in an LCM,
  determines which LCM (which components of the response vector
  are conditioned), and fits the MLE in the LCM.

  The function \code{\link{summary}} (i.e., \code{\link{summary.llmdr}})
  can be used to obtain or print a summary of the results.

  For multinomial and product multinomial, this function does not work
  like other multinomial regression functions, for example,
  \code{\link[nnet]{multinom}}.  Instead the response vector is just
  a vector of counts, like in Poisson regression.  We actually fit
  the Poisson model with \code{conditioning} added as a term on the
  right-hand side of \code{formula} and then do not report the coefficients
  for the levels of \code{conditioning} as these are not identifiable
  if the sampling model is actually product multinomial.
  If \code{conditioning} is missing, we actually fit the Poisson model with
  an intercept added to \code{formula} if not present already and then
  do not report the \code{(Intercept)} coefficient as it is not identifiable
  if the sampling model is actually multinomial.

  Adjustments are made to the output of R functions \code{\link{summary}},
  \code{\link{anova}}, \code{\link{predict}}, \code{\link{vcov}}, and so
  forth to account for the sampling being multinomial or product multinomial
  rather than Poisson and also to account for the MLE being in a limiting
  conditional model (if it is).
}

\value{
  \code{llmdr} returns an object of class inheriting from \code{"llmdr"},
  which is a list containing at lest the following components:
 
  \item{family}{argument \code{family}.}
  \item{conditioning}{argument \code{conditioning}.}
  \item{is.lcm}{logical.  \code{TRUE} indicates that the MLE for the
      regression coefficients does not exist and the model fit is
      the LCM.  \code{FALSE} indicates that the MLE for the original
      model does exist and the model fit is the original model.}
  \item{is.fixed.lcm}{a logical vector that says which components of
      the response vector are conditioned to be equal to their observed
      values in the LCM.  Not present when component \code{is.lcm} is
      \code{FALSE}.}
  \item{gdor}{a generalized direction of recession (for which see
      Geyer, 2009).  Not present when component \code{is.lcm} is
      \code{FALSE}.}
  \item{glm}{an object of class \code{"glm"}, the result of using
      R function \code{\link{glm}} to fit the OM or LCM as the case
      may be.}
  \item{conditioning.coefficients}{names of coefficients of the \code{glm}
      component that should not be reported because they are not identifiable
      in a multinomial or product multinomial model.}
}
\seealso{
  \code{\link{summary.llmdr}}, etc.
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
2 + 2
}
\keyword{models}
\keyword{regression}
