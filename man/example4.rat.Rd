\name{example4.rat}
\alias{example4.rat}
\docType{data}
\title{
Example 4 of rating data with some missing data
}
\description{
The result of \code{readratdatafixed("<example4.rat.txt>")}. A 7-point rating scale has been used. Numbers of assessors and assessed are equal (10). Each respondent is identified by a schoolid, a group id and a respondent id. The rows contain the assessors, the columns contain the assessed.
When rater equals ratee (diagonal), the rating is NA.
}
\usage{data("example4.rat")}
\format{
  A data frame with 10 observations of 9 ratings; some of the ratings are missing.
  \describe{
    \item{\code{schoolid}}{a numeric vector, identifying the second group level}
    \item{\code{groupid}}{a numeric vector, identifying the first group level.}
    \item{\code{respid}}{a numeric vector, identifying the individual.}
    \item{\code{r01}}{ratings received by respondent 1.}
    \item{\code{r02}}{ratings received by respondent 2.}
    \item{\code{r03}}{ratings received by respondent 3.}
    \item{\code{r04}}{ratings received by respondent 4.}
    \item{\code{r05}}{ratings received by respondent 5.}
    \item{\code{r06}}{ratings received by respondent 6.}
    \item{\code{r07}}{ratings received by respondent 7.}
    \item{\code{r08}}{ratings received by respondent 8.}
    \item{\code{r09}}{ratings received by respondent 9.}
    \item{\code{r10}}{ratings received by respondent 10.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\note{Rating data can be entered directly into a SSrat compliant dataframe, using \code{\link{edit}}. Colums needed are: "schoolid", "groupid", "respid", and for <n> raters "r01", "r02".."r<n>". Optionally, a column named "resplabel" can be entered, containing an additional identifier of the raters/assessed. The raters (assessors) are in rows and assessed in columns. For example: \cr
mydata=data.frame(schoolid=numeric(0), groupid=numeric(0), respid=numeric(0),\cr
       r01=numeric(0), r02=numeric(0), r03=numeric(0)); mydata=edit(mydata)
}
\seealso{
\code{\link{readratdatafixed}}
\code{\link{example1.rat}}
\code{\link{example1a.rat}}
\code{\link{example2.rat}}
\code{\link{example3.rat}}
%%\code{\link{example4.rat}}
\code{\link{example5.rat}}
\code{\link{example6.rat}}
\code{\link{example7.rat}}
\code{\link{klas2.rat}}
}
\examples{
data(example4.rat)
}
\keyword{datasets}
