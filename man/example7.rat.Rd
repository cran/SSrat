\name{example7.rat}
\alias{example7.rat}
\docType{data}
\title{
Example 7 of rating data of three groups and two different rating scales
}
\description{The combined data matrices of three groups. Because the rating scale is 5 points for the groups (1, 1) and (2, 1), while a 7 points rating scale has been used in group (1, 3), function calcallgroups cannot be used for further processing. In stead, calcgroups must be used for each group seperately.\cr
The result of readratdatafixed("<example7.rat.txt>"). Each respondent is identified by a schoolid, a group id and a respondent id. The rows contain the assessors, the columns contain the assessed.
When rater equals assessed (diagonal), the rating is NA.
}
\usage{data("example7.rat")}
\format{
  A data frame with 10 observations of 9 ratings.
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
       r01=numeric(0), r02=numeric(0), r03=numeric(0)); mydata=edit(mydata)\cr
       To allow for the combination of groups with different sizes in a single file, it is important to enumerate the n respondents from 1 to n, respectively use the columnnames r01 to rn for the received ratings. These column names are padded with a zero (r01, r02 etc.) to allow for easy ordening of these columns after a merge.
}
\seealso{
\code{\link{readratdatafixed}}
\code{\link{calcallgroups}}
\code{\link{calcgroup}}
\code{\link{example1.rat}}
\code{\link{example1a.rat}}
\code{\link{example2.rat}}
\code{\link{example3.rat}}
\code{\link{example4.rat}}
\code{\link{example5.rat}}
\code{\link{example6.rat}}
%%\code{\link{example7.rat}}
\code{\link{klas2.rat}}
}
\examples{
data(example7.rat)
}
\keyword{datasets}
