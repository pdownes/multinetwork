\name{build.networks}
\alias{build.networks}
\title{Build Networks List}
\description{
This function creates a networks list from survey output data. It is optimized for data in the format collected after running make.input (see make.input).}
\usage{
build.networks(data, tielist, groupcol, indcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Survey data to be built into networks object. Best used after running make.input and collecting data.}
  \item{tielist}{
A list format of columns in collected data where tie values can be found. Best used with relation names (see  examples).}
  \item{groupcol}{
The column number that has group identifiers.}
  \item{indcol}{
The column number that has individual identifiers.}
}
\value{
Returns a networks list
  \item{relation1 }{List of relation 1 graphs}
  \item{relation2 }{List of relation 2 graphs}
}

\author{Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu> }

\seealso{
	\code{\link{make.input}}
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
}

