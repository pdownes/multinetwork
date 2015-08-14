\name{symmetrize.networks}
\alias{symmetrize.networks}
\title{Symmetrize Networks}
\description{Symmetrizes all matrices in the networks object according to the chosen method.}
\usage{
symmetrize.networks(networks, method = c("max", "min", "mean", "sum"), na.to.zero = FALSE)
}
\arguments{
  \item{networks}{A networks list.}
  \item{method}{The method of symmetrization: "max" replaces the tie with the maximum of the two directed values, "min" replaces the tie with the minimum of the two directed values, "mean" takes the average of the two directed values and "sum" adds the two directed values together to get an undirected total.}
  \item{na.to.zero}{By default, any NA in the matrix will result in symmetrization of the NA, regardless of the method chosen. Setting na.to.zero=T will first replace all NAs with zero then run the symmetrization method chosen.}
}

\value{A dichotomized networks list.}
\author{Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu> }
\examples{
# First build a networks list from collected data.
input.data <-data.frame(a=1,b=2,ind=1:20,c=3,d=4,group=c(rep(1:4,each=3),rep(5:6,each=4)))#made-up data
collected.data<-make.input(input.data,groupcol=6,altercols=c(3:5))#make input for data collection

collected.data$a1friend <- rep(1:4,5)#made-up data collection
collected.data$a2friend <- rep(4:1,5)#made-up data collection
collected.data$a1trust <- rep(4:1,5)#made-up data collection
collected.data$a2trust <- rep(1:4,5)#made-up data collection
collected.data$a3trust <- rep(5:1,4)#made-up data collection
collected.data$a3friend <- c(rep(8,8),rep(9,8),rep(7,4))#made-up data collection

tielist <- list(friend=c(16,17,21),trust=18:20)#build tielist
networks <- build.networks(collected.data,tielist,6,3)#build networks

# Symmetrize Function
sn <- symmetrize.networks(networks,method="max")

}