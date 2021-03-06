\name{crabs}
\docType{data}
\alias{crabs}
\title{Horseshoe Crab Mating Data}
\usage{data(crabs)}
\description{
  Data on horseshoe crabs (\emph{Limulus polyphemus}).  Response
  is number of males surrounding a breeding female, color (factor), condition
  (factor), weight (quantitative), and width (quantitative) of the female.
}
\format{
  A data frame with 173 observations on 6 variables.
  Individuals (rows of the data frame) are female horseshoe crabs.
  Variables other than \code{satell} refer to these females.
  The variables are
  \describe{
  \item{color}{color. The colors given in
    Agresti are \dQuote{light medium}, \dQuote{medium}, \dQuote{dark medium},
    and \dQuote{dark}.  Here they are abbreviated to \code{light},
    \code{medium}, \code{dark}, and \code{darker}, respectively.}
  \item{spine}{spine condition.  The conditions given in Agresti are
    \dQuote{both good}, \dQuote{one worn or broken}, and
    \dQuote{both worn or broken}.
    Here they are abbreviated to \code{good}, \code{middle}, \code{bad},
    respectively.}
  \item{width}{carapace width in centimeters}
  \item{satell}{number of satellites, which males clustering around the
    female in addition to the male with which she is breeding.}
  \item{weight}{weight in grams.}
  \item{y}{shorthand for \code{as.numeric(satell > 0)}.}
  }
}
\details{
Quoting from the abstract of Brockmann (1996). \dQuote{Horseshoe crabs
arrive on the beach in pairs and spawn \ldots during \ldots high tides.
Unattached males also come to the beach, crowd around the nesting couples
and compete with attached males for fertilizations. Satellite males form
large groups around some couples while ignoring others, resulting in
a nonrandom distribution that cannot be explained by local environmental
conditions or habitat selection.}
}
\source{
  Agresti, A. (2013)
  \emph{Categorical Data Analysis},
  Wiley, Hoboken, NJ., Section 4.3.2,
  \url{http://users.stat.ufl.edu/~aa/cda/data.html}

  Brockmann, H. J. (1996)
  Satellite Male Groups in Horseshoe Crabs, \emph{Limulus polyphemus},
  \emph{Ethology}, \bold{102}, 1--21.
}
\examples{
  data(crabs)
  gout <- glm(satell ~ color + spine + width + weight, family = poisson,
      data = crabs)
}

