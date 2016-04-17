\name{isGraphical}
\alias{asGraphical}
\alias{isGraphical}
\alias{asHierarchical}
\alias{isHierarchical}
\title{Hierarchical and Graphical Models}
\usage{
asGraphical(formula)
isGraphical(formula)
asHierarchical(formula)
isHierarchical(formula)
}
\description{
  Say whether a formula corresponds to a hierarchical model or
  a graphical model.  Or return a formula for a hierarchical or
  a graphical model.
}
\arguments{
  \item{formula}{an object of class \code{"\link[stats]{formula}"}.}
}
\value{
  For \dQuote{is} functions, logical.  \code{TRUE} if and only if
  the model is hierarchical or graphical, as the case may be.

  For \dQuote{as} functions, a formula for the smallest supermodel
  of the given model that is hierarchical or graphical, as the case may be.
}
\details{
A model is hierarchical if for every interaction it contains all
the main effects and lower-order interactions for variables in that
interaction.

The interaction graph of a model is the undirected graph whose node set is the
predictor variables in the model and whose edge set has one edge for each
pair of variables that are in an interaction term.  A clique in a graph is
a maximal complete subgraph.  A model is graphical if it is hierarchical
and has an interaction term for the variables in each clique.
}
\examples{
isHierarchical(~ u * v)
isHierarchical(~ u : v)

isGraphical(~ u * v + u * w)
isGraphical(~ (u + v + w)^2)

asHierarchical(~ u:v + v:w)
asGraphical(~ (u + v + w)^2)
}
\keyword{misc}
