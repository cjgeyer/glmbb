\name{glmbb}
\alias{glmbb}
\title{All Hierarchical or Graphical Models for Generalized Linear Model}
\usage{
glmbb(big, little = ~ 1, family = poisson, data,
    criterion = c("AIC", "AICc", "BIC"), cutoff = 10,
    trace = FALSE, graphical = FALSE, BIC.option = c("length", "sum"), \dots)
}
\description{
  Find all hierarchical submodels of specified GLM with
  information criterion (AIC, BIC, or AICc) within specified cutoff
  of minimum value.  Alternatively, all such graphical models.
  Use branch and bound algorithm so we do not have to fit all
  models.
}
\arguments{
  \item{big}{an object of class \code{"\link[stats]{formula}"} specifying
    the largest model to be considered.  Model specified must be hierarchical.
    (See also \code{\link[stats]{glm}} and \code{\link[stats]{formula}} and
    \sQuote{Details} section below.)}

  \item{little}{a formula specifying the smallest model to be considered.
    The response may be omitted and if not omitted is ignored (the response
    is taken from \code{big}).  Default is \code{~ 1}. Model specified must
    be nested within the model specified by \code{big}.}

  \item{family}{a description of the error distribution and link
    function to be used in the model.  This can be a
    character string naming a family function, a family function or the
    result of a call to a family function.  (See \code{\link[stats]{family}}
    for details of family functions.)}

  \item{data}{an optional data frame, list or environment (or object
    coercible by \code{\link[base]{as.data.frame}} to a data frame)
    containing the variables in the models.  If not found in \code{data}, the
    variables are taken from \code{environment(big)},
    typically the environment from which \code{glmbb} is called.}

  \item{criterion}{a character string specifying the information criterion,
    must be one of \code{"AIC"} (Akaike Information Criterion, the default),
    \code{"BIC"} (Bayes Information Criterion) or \code{"AICc"} (AIC corrected
    for sample size).   See section on AICc below.}

  \item{cutoff}{a nonnegative real number. This function finds all
    hierarchical models that are submodels of \code{big} and supermodels of
    \code{little} with information criterion less than or equal to the
    cutoff plus the minimum information criterion over all these models.}

  \item{trace}{logical.  Emit debug info if \code{TRUE}.}

  \item{graphical}{logical.  If \code{TRUE} search only over graphical models
      rather than hierarchical models.}

  \item{BIC.option}{a character string specifying the sample size \eqn{n}
      to be used in calculating BIC (ignored if \code{criterion ! = "BIC"}),
      must be either \code{"length"} or \code{"sum"} meaning either the
      length of the response vector (or number of rows if the response
      \dQuote{vector} is actually a matrix) or the sum of the response
      (number of individuals classified if we are doing categorical
      data analysis).  See section about BIC below.  May be abbreviated.}

  \item{\dots}{additional named or unnamed arguments to be passed
    to \code{\link[stats]{glm}}.}
}
\details{
  Typical value for \code{big} is something like \code{foo ~ bar * baz * qux}
  where \code{foo} is the response variable (or matrix when family is
  \code{\link[stats]{binomial}} or \code{\link[stats]{quasibinomial}},
  see \code{\link[stats]{glm}}) and \code{bar}, \code{baz}, and \code{qux}
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
  no intercept by \code{0 +} or some such (see \code{\link[stats]{formula}}).

  The interaction graph of a model is the undirected graph whose node set is
  the predictor variables in the model and whose edge set has one edge for each
  pair of variables that are in an interaction term.  A clique in a graph is
  a maximal complete subgraph.  A model is graphical if it is hierarchical
  and has an interaction term for the variables in each clique.
  When \code{graphical = TRUE} only graphical models are considered.
}
\section{BIC}{
It is unclear what the sample size, the \eqn{n} in the BIC penalty
\eqn{n \log(p)}{n log(p)} should be.  Before version 0.4 of this package
the BIC was taken to be the result of applying R generic function \code{BIC}
to the fitted object produced by R function \code{glm}.  This is generally
wrong whenever we think we are doing categorical data analysis
(Raftery, 1986; Kass and Raftery, 1995).  Whether we consider the sampling
scheme to be Poisson, multinomial, or product multinomial (and binomial
is a special case of product multinomial) the sample size is the total
number of individuals classified and is the only thing that is
considered as going to infinity in the usual asymptotics for categorical
data analysis.  This the option \code{BIC.option = "sum"} should always
be used for categorical data analysis.
}
\section{AICc}{
AICc was derived by Hurvich and Tsai only for normal response models.
Burnham and Anderson (2002, p. 378) recommend it for other models when
no other small sample correction is known, but this is not backed up
by any theoretical derivation.
}
\value{
  An object of class \code{"glmbb"} containing at least the following
  components:
  \item{data}{the model frame, a data frame containing all the variables.}
  \item{little}{the argument \code{little}.}
  \item{big}{the argument \code{big}.}
  \item{criterion}{the argument \code{criterion}.}
  \item{cutoff}{the argument \code{cutoff}.}
  \item{envir}{an R environment object containing all of the fits done.}
  \item{min.crit}{the minimum value of the criterion.}
  \item{graphical}{the argument \code{graphical}.}
}
\references{
  Burnham, K. P. and Anderson, D. R. (2002)
  \emph{Model Selection and Multimodel Inference: A Practical
      Information-Theoretic Approach}, second edition.
  Springer, New York.

  Hand, D. J. (1981)
  Branch and bound in statistical data analysis.
  \emph{The Statistician}, \bold{30}, 1--13.

  Hurvich, C. M. and Tsai, C.-L. (1989)
  Regression and time series model selection in small samples.
  \emph{Biometrika}, \bold{76}, 297--307.

  Kass, R. E. and Raftery, A. E. (1995)
  Bayes factors.
  \emph{Journal of the American Statistical Association},
    \bold{90}, 773--795.

  Raftery, A. E. (1986)
  A note on Bayes factors for log-linear contingency table models
      with vague prior information.
  \emph{Journal of the Royal Statistical Society, Series B},
    \bold{48}, 249--250.
}
\seealso{
  \code{\link[stats]{family}},
  \code{\link[stats]{formula}},
  \code{\link[stats]{glm}},
  \code{\link{isGraphical}},
  \code{\link{isHierarchical}}
}
\examples{
data(crabs)
gout <- glmbb(satell ~ (color + spine + width + weight)^3,
    criterion = "BIC", data = crabs)
summary(gout)
}
\keyword{models}
\keyword{regression}
\concept{model selection}
\concept{AIC}
\concept{BIC}
