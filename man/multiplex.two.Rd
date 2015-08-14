\name{multiplex.two}
\alias{multiplex.two}
\title{Multiplex Two Relations}
\description{Conducts a multiplex routine on a networks object and returns another networks object. The return networks object will have 3 relations: relation 1 only, relation 2 only, and both.}
\usage{
multiplex.two(networks, relation1, relation2)
}

\arguments{
  \item{networks}{A networks list.}
  \item{relation1}{The name of the first relation to include in the routine.}
  \item{relation2}{The name of the second relation to include in the routine.}
}
\value{A networks list with 3 relations.}
\author{Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu> }
\seealso{
 \link{multiplex.three}
}

\examples{three.way.multigraph <- list(
  R1 = list(
    one=matrix(c(NA,1,1,1,0,NA,0,1,0,0,NA,0,0,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,0,1,0,NA,1,0,0,0,NA,0,1,0,0,NA),nrow=4,ncol=4)
    ),
  R2 = list(
    one=matrix(c(NA,1,1,0,0,NA,1,0,0,0,NA,1,0,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,1,1,0,NA,0,1,0,0,NA,0,0,0,0,NA),nrow=4,ncol=4)
    ),
  R3 = list(
    one= matrix(c(NA,1,0,1,0,NA,1,0,0,0,NA,0,1,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,1,0,0,NA,1,0,0,0,NA,1,0,0,0,NA),nrow=4,ncol=4)
    )
)#made-up data

mn <- multiplex.two(three.way.multigraph,1,2)#create multiplex output
}
