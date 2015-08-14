\name{multiplex.three}
\alias{multiplex.three}
\title{Multiplex Three Relations}
\description{
This function conducts a multiplex routine on a networks object and returns another networks object. The return networks object will have 7 relations: relation 1 only, relation 2 only, relation3 only, relations 1 & 2, relations 2 & 3, relations 1 & 3, and all relations.}
\usage{
multiplex.three(networks, relation.list)
}
\arguments{
  \item{networks}{A networks list.}
  \item{relation.list}{A list of relations to include in the analysis (using relation names if relation names are used in the networks object)}
}

\value{A networks list with 7 relations.}
\author{Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu> }
\seealso{
 \code{\link{multiplex.two}}
}
\examples{
three.way.multigraph <- list(
  R1 = list(
    one=matrix(c(NA,1,1,1,0,NA,0,1,0,0,NA,0,0,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,0,1,0,NA,1,0,0,0,NA,0,1,0,0,NA),nrow=4,ncol=4),
    three= matrix(c(NA,1,1,0,NA,1,0,0,NA),nrow=3,ncol=3)
    ),
  R2 = list(
    one=matrix(c(NA,1,1,0,0,NA,1,0,0,0,NA,1,0,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,1,1,0,NA,0,1,0,0,NA,0,0,0,0,NA),nrow=4,ncol=4),
    three= matrix(c(NA,1,0,1,NA,1,0,1,NA),nrow=3,ncol=3)
    ),
  R3 = list(
    one= matrix(c(NA,1,0,1,0,NA,1,0,0,0,NA,0,1,0,0,NA),nrow=4,ncol=4),
    two= matrix(c(NA,1,1,0,0,NA,1,0,0,0,NA,1,0,0,0,NA),nrow=4,ncol=4),
    three= matrix(c(NA,0,1,1,NA,0,1,0,NA),nrow=3,ncol=3)
    )
)#made-up datarelation.list<-names(three.way.multigraph) #create relation list (use all 3 in this example)mn<- multiplex.three(three.way.multigraph,relation.list)#create multiplex output
}
