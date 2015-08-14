\name{dichotomize.networks}
\alias{dichotomize.networks}
\title{Dichotomize Networks}
\description{
Dichotomizes a networks object, which is required for certain computations.}
\usage{
dichotomize.networks(networks, method = c("grand.mean", "relation.mean", "team.mean", "value", "specific.list"), value = NULL, specific.list = NULL)
}
\arguments{
  \item{networks}{
 The networks list to be dichotomized
}
  \item{method}{
Dichotomization method: "grand.mean" breaks ties at the mean of all ties in the object, "tie.mean" breaks ties at the mean of all ties of that type (e.g. friendship), "net.mean" breaks ties at the mean of the specific network (within a relation), "value" breaks ties at the user-entered value, "specific.list" allows for advanced dichotomization. A "specific.list" format mirrors a tielist by producing a value of dichotomization for each network/relation instance. In all methods, ties over the comparator value are set to 1 while ties under the comparator value are set to 0.}
  \item{value}{The value at which to dichotomize. Only used for "value" method.}
  \item{specific.list}{
A comparator list that mirrors a networks list providing 1 value for each network/relation instance to be  dichotomized around.}
}
\details{
}
\value{
A dichotomous networks list.
}
\references{
}
\author{
Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu>
}

\seealso{

}
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

# Dichotomize Function
dich.networks1 <- dichotomize.networks(networks,method="grand.mean")dich.networks2 <- dichotomize.networks(networks,method="value",value=4)#create and use a complex dichotomization procedurespecific.list <- list(friend=list(2,2,2,2,3,3),trust=list(2,2,2,2,3,3))dich.networks3 <- dichotomize.networks(networks,method="specific.list",specific.list=specific.list)
}