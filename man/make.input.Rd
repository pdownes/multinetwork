\name{make.input}
\alias{make.input}
\title{
Make Survey Input}
\description{
This function creates a spreadsheet format for easy upload into a survey software of your choice. It prepends "a" to alter data columns so those values can be piped into the survey software (e.g. Qualtrics, SurveyMonkey). After data collection, run build.networks to create a networks object for analysis.}
\usage{
make.input(data, groupcol = 2, altercols = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Input data with a group identifier and alter columns.}
  \item{groupcol}{
The column number that has group identifiers.
}
  \item{altercols}{
The column numbers of relevant alter data to be added.}
}
\details{
}
\value{
A dataframe.
}
\references{
}
\author{
Patrick Downes <pat-downes@uiowa.edu>, Eean Crawford <eean-crawford@uiowa.edu> }

\note{
}

\seealso{
}
\examples{
input.data<-data.frame(a=1,b=2,ind=1:20,c=3,d=4,group=c(rep(1:4,each=3),rep(5:6,each=4)))#made-up datacollected.data<-make.input(input.data,groupcol=6,altercols=c(3:5))#reformat for data collection
}