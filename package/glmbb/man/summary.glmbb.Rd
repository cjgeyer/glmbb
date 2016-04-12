\name{summary.glmbb}
\alias{summary.glmbb}
\alias{print.summary.glmbb}
\title{Summarize GLM Model Selection via Branch and Bound}
\usage{
\method{summary}{glmbb}(object, cutoff, \dots)

\method{print}{summary.glmbb}(x, digits = max(3, getOption("digits") - 3),
    \dots)
}
\arguments{
  \item{object}{an object of class \code{"glmbb"}, usually, a result of a
    call to \code{\link{glmbb}}.}
  \item{cutoff}{a nonnegative real number. Only report on models having
    criterion value no larger than the minimum value plus \code{cutoff}.
    This argument may be omitted, in which case \code{object$cutoff} is
    used.}
  \item{x}{an object of class \code{"summary.glmbb"}, usually, a result of a
    call to \code{summary.glmbb}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{not used.  Required by their generics.}
}
\description{
  These functions are all \code{\link[utils]{methods}}
  for class \code{glmbb} or \code{summary.glmbb} objects.
}
\details{ 
  Let \code{criterion} denote the vector of criterion (AIC, BIC, or AICc)
  values for all of the models evaluated in the search.  Those with
  criterion value greater than \code{min(criterion) + cutoff} are tossed.

  We also define a vector \code{weight} by
  \preformatted{w <- exp(- criterion / 2)
weight <- w / sum(w)}
  except that it is calculated differently to avoid overflow.
  These are so-called Akaike weights.  They may or may not provide some
  guide as to how to deal with these models.  For more see Burnham and
  Anderson (2002).
}
\value{
  \code{summary.glmbb} returns an object of class \code{"summary.glmbb"}, a
  list with components

  \item{results}{a data frame having variables
    \describe{
      \item{\code{criterion}}{the vector \code{criterion} described
        in the Details section, in sorted order.}
      \item{\code{weight}}{the corresponding Akaike weights.}
      \item{\code{formula}}{the corresponding formulas describing
        the corresponding models.}
    }
  }
  \item{cutoff.search}{the \code{cutoff} argument to the call to \code{glmbb}
    that produced \code{object}.}
  \item{cutoff.summary}{the \code{cutoff} argument to the call to
    \code{summary.glmbb}.}
  \item{criterion}{a character variable giving the name of the criterion
    (AIC, BIC, or AICc). Not to be confused with \code{results$criterion}.}
}
\references{
Burnham, K. P. and Anderson, D. R. (2002).
\emph{Model Selection and Multimodel Inference: A Practical
    Information-Theoretic Approach}, 2nd ed.
Springer-Verlag, New York.
}
\examples{
## For examples see those in help(glmbb)
}
\keyword{models}
\keyword{regression}
