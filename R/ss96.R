#' %% ~~ data name/kind ... ~~ Rating data of 8 groups from a Dutch primary
#' school, year 1996
#' 
#' Rating data of 1996. Measurements of these Dutch primary school children
#' were taken three times, in 1996, 1997 and 1998. In 1996, the sample
#' consisted of 193 students (91 boys, 96 girls, six gender unknown); 189
#' students were involved in 1997 (90 boys, 95 girls, 4 gender unknown); and in
#' 1998, 189 students participated (91 boys, 98 girls). Average age in 1998 was
#' 11.3 years, with ages ranging from 11 to 13 years. The participants were
#' asked to rate all their classmates on a seven-point scale: dislike very
#' much, dislike much, dislike, ordinary, like, like much, and like very much.
#' %% ~~ A concise (1-5 lines) description of the dataset. ~~
#' 
#' A data frame with 193 observations of 32 variables, with (a maximum of) 29
#' ratings (r01..r29).  \code{schoolid} a numeric vector, identifying the
#' second group level; \code{groupid} a numeric vector, identifying the first
#' group level; \code{respid} a numeric vector, identifying the individual;
#' \code{r01:r29} ratings received by respondent 1..29.
#' 
#' @name Ss96.rat
#' @docType data
#' @seealso \code{\link{Ss97.rat}} \code{\link{Ss98.rat}} \code{\link{bg}}
#' @source Maassen, G.H., Van Boxtel, H.W., & Goossens, F.A. (2005).
#' Reliabilty of nomination and two-dimensional rating scale methods for
#' sociometric status determination. Journal of Applied Developmental
#' Psychology, 26, 51-68.
#' @keywords datasets
#' @examples
#' 
#' data(Ss96.rat)
#' 
NULL
