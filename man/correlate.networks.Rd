\name{correlate.networks}
\alias{correlate.networks}
\title{
Correlate Networks}
\description{
Computes a Pearson correlation coefficient within groups across all possible combinations of relations. The function expects a networks list and returns a dataframe.}
\usage{
correlate.networks(networks)
}
\arguments{
  \item{networks}{
A networks list.}
}

\value{
A dataframe in which rows represent teams and columns represent correlations between two relations. If relations are named in the networks object the column headings will be easily interpretable.
}

\author{
Patrick Downes <pat-downes@uiowa.edu>}


\seealso{
 \code{\link{analyze.networks}}
}
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

correlate.networks(networks) #calculate pearson correlations
}