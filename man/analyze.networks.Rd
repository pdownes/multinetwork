\name{analyze.networks}
\alias{analyze.networks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analyze Networks}
\description{Calculates a variety of graph-level indices.}
\usage{
analyze.networks(networks, ...)
}
\arguments{
  \item{networks}{A built networks list.}
  \item{\dots}{Not used in this version.}
}
\value{
A dataframe in which rows represent teams and columns represent various network indices. Currently returns density, number of clusters, degree centralization (all, in, and out computations), betweenness centralization and closeness centralization.
}
\author{Patrick E. Downes <pat-downes@uiowa.edu>}
\seealso{package 'igraph'}

\examples{
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

analyze.networks(networks) #conduct analysis
}
