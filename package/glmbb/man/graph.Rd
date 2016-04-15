\name{isGraphical}
\alias{isGraphical}
\alias{isHierarchical}
\title{Hierarchical and Graphical Models}
\usage{
isGraphical(formula)
isHierarchical(formula)
}
\description{
  Say whether a formula corresponds to a hierarchical model or
  a graphical model.
}
\arguments{
  \item{formula}{an object of class \code{"\link[stats]{formula}"}.}
}
\value{
  Logical.  \code{TRUE} if and only if the model is hierarchical or
  graphical.
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
}
\keyword{misc}
