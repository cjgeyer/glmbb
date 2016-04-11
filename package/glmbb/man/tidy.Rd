\name{tidy.formula.hierarchical}
\alias{tidy.formula.hierarchical}
\title{Shorten a Hierarchical Formula}
\usage{
tidy.formula.hierarchical(formula)
}
\description{
  Simplify a formula, assuming it is hierarchical, that is,
  an interaction implies all lower-order interactions and main
  effects involving the same variables are in the model.
}
\arguments{
  \item{formula}{an object of class \code{"\link[stats]{formula}"}.}
}
\value{
  Another formula.
}
\examples{
tidy.formula.hierarchical(y ~ u + v + w + u:v + u:w + v:w + u:v:w)
}
\keyword{misc}
