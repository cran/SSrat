\name{describeratdata}
\alias{describeratdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show various features of the SSrat dataframe
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
describeratdata shows the following features: Number of unique school, number of unique groups of raters/assessed, and for each group, the school to which the group belongs, the group identifier, the ratings that are actually used, the count of assessors, the count of assessed, the number of missing ratings, the bias in the ratings, the number of ratings less than the midrating, the number of ratings equal to the midrating, the number of ratings larger than the midrating. Function describeratdata is useful for a first check of the ratings./cr
N.B. Unavailable values (NA) on the diagonal are not counted as mssing ratings.
}
\usage{
describeratdata(dataframe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
The dataframe should have the following columns: schoolid groupid respid, and for n raters the columns r01 .. rn, with a maximum of r99. Function \code{\link{readratdatafixed}} creates an appropriate data frame from a text file with ratings.
%%     ~~Describe \code{dataframe} here~~
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A data.frame with for each group:
\item{schoolid }{The school identifier}
\item{groupid }{The group identifier}
\item{scores }{The scores that have been used by the raters}
\item{assessors }{The number of assessors}
\item{assessed }{The number of assessed}
\item{missing }{The number of missing ratings, not counting the missings on the diagonal}
\item{bias }{The bias in the ratings, defined as the mean of the ratings minus the midscore value (R)}
\item{ltR }{The number of ratings, less than the midscore value (R)}
\item{R }{The number of ratings, equal to the midscore value (R)}
\item{gtR }{The number of ratings, greater than the midscore value (R)}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Hans Landsheer
%%  ~~who you are~~
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{readratdatafixed}}

}
\examples{
data(example6.rat)
describeratdata(example6.rat)
data(example7.rat)
describeratdata(example7.rat)
data(example1a.rat)
describeratdata(example1a.rat)
}
\keyword{utilities}
\keyword{Utilities}% __ONLY ONE__ keyword per line
