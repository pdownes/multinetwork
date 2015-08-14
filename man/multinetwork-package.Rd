\name{multinetwork-package}
\alias{multinetwork-package}
\alias{multinetwork}
\docType{package}
\title{Network Analysis for Teams}
\description{This has data formatting and basic network analysis procedures for analyzing network structures of small groups and teams.}
\details{
 \tabular{ll}{
  Package: \tab multinetwork\cr
  Type: \tab Package\cr
  Version: \tab 0.11\cr
  Date: \tab 2015-08-12\cr
  License: \tab GPU-2\cr
 }
}
\author{
Patrick E. Downes, Eean R. Crawford
Maintainer: Patrick E. Downes <pat-downes@uiowa.edu>
}
\keyword{ multinetwork }
\seealso{
 \code{\link[igraph:igraph-package]{igraph}}
}
\note{
Networks List: At the core of multinetwork is the networks List. multinetwork builds and expects data to be in the networks list format. This format has top-level elements representing relations, tie types or tie content (e.g. "friendship ties", "trust ties"). Sub-elements are a list of n x n matrices representing each network. Sub-elements are ordered in parallel such that the first element in the friendship list is the same team represented by the first element in the trust list. 
}
