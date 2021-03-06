#' Reads the ratings of a group of raters from a specified textfile into a
#' SSrat compliant dataframe
#' 
#' Given the ratings of a group in a specified textfile, readratdatafixed
#' produces a dataframe that can be processed further with SSrat, using
#' function calcgroup.\cr
#' 
#' Example text file "example1.rat.txt" has the following format:\cr 010101
#' .717211723\cr 010102 7 72121714\cr 010103 76 7231711\cr 010104 771 141756\cr
#' 010105 7672 51171\cr 010106 77172 1123\cr 010107 767216 711\cr 010108
#' 7717211 45\cr 010109 76721711 6\cr 010110 771722112 \cr
#' 
#' schoolid has position c(1,2), groupid has position c(3,4), respid has
#' position c(5,6), and rating has position c(8,17)
#' 
#' The ratings form often a square (i.e., number of raters is equal to the
#' number of assessed), but the number of raters can be larger or smaller than
#' the number of assessed. See example2.rat.txt for number of raters larger
#' than the number of ratees. See example3.rat.txt for number of raters smaller
#' than the number of ratees.\cr Ratings can be MISSING, identified by a zero
#' or a non-numeric character. In example1.rat.txt the missing value indicator
#' is a space. File klas2.rat.txt provides another example.\cr Raters can be in
#' columns. Set rowsEQassessors = FALSE. See example4.rat.txt.\cr Multiple
#' groups can be processed by radratdatafixed in a single run. It is
#' recommended that the groups are divided by an empty line. The positions of
#' the fields need to be the same for each group. File example6.rat.txt.
#' example7.rat.txt and example8.rat.txt provide examples. A string that
#' identifies each respondent can be entered optionally. File example1a.rat.txt
#' and klas2.rat.txt provide examples.
#' 
#' @param filename the filename of the textfile to be processed
#' @param pnames optional: the fixed postion of the names of the
#' raters/assessed. Default: c(0, 0), not available.
#' @param pschoolid the fixed position of a number that identifies the school.
#' @param pgroupid the fixed position of a number that identifies the group.
#' @param prespid the fixed position of a number that identifies the rater.
#' @param pratings the fixed position of the ratings, where each rating is a
#' number of the set {1, 2, 3}, {1..5}, {1..7} or {1..9}. When there are 10
#' raters, the positions covers 10 single digit numbers.
#' @param rowsEQassessors A boolean that identifies whether the raters
#' (assessors) are in rows or not. Default is TRUE, the assessors are in rows.
#' @return The output is a dataframe that can be processed further by SSrat.\cr
#' File example1.rat.txt produces a dataframme with the following columns:\cr
#' schoolid groupid respid r01 r02 r03 r04 r05 r06 r07 r08 r09 r10\cr and NA
#' where missing values are detected.
#' 
#' N.B. Wrong identification of the various field positions easily leads to a
#' dataframe with unexpected missing values (NA's).
#' @note Rating data can be entered directly into a SSrat compliant dataframe,
#' using Edit. Colums needed are: "schoolid", "groupid", "respid", and for <n>
#' raters "r01", "r02".."r<n>", with a maximum of r99. Optionally, a column
#' named "resplables" can be entered, containing an extra string identifier of
#' the raters/assessed. The raters (assessors) are in rows and assessed in
#' columns.
#' @author Hans Landsheer
#' @seealso \code{\link{calcgroup}}
#' @references Maassen, G. H. and Landsheer, J. A. (1998). SSRAT: The
#' processing of rating scales for the determination of two-dimensional
#' sociometric status. Behavior Research Methods Instruments & Computers,
#' 30(4), 674-679.
#' @keywords datasets
#' @examples
#' 
#' #example file names
#' filenames = c("example1.rat.txt","example2.rat.txt","example3.rat.txt",
#'               "example4.rat.txt","example5.rat.txt","example6.rat.txt",
#'               "example7.rat.txt","example1a.rat.txt","klas2.rat.txt")
#' filenames=paste(path.package("SSrat"),"/extdata/",filenames, sep="") 
#' 
#' # show raw text file
#' cat( readLines( filenames[1] ) , sep = "\n" )
#'               
#' #single group, no names, standard positions
#' (example1.rat = readratdatafixed(filenames[1]))
#' (example2.rat = readratdatafixed(filenames[2])) # raters < assessed
#' (example3.rat = readratdatafixed(filenames[3])) # raters > assessed
#' (example4.rat = readratdatafixed(filenames[4], rowsEQassessors = FALSE)) # raters in columns
#' (example5.rat = readratdatafixed(filenames[5]))
#' 
#' 
#' #multiple groups
#' (example6.rat = readratdatafixed(filenames[6]))
#' (example7.rat = readratdatafixed(paste(filenames[7])))
#' 
#' #single groups, with names: define positions
#' (example1a.rat = readratdatafixed(filenames[8], 
#'                            pnames=c(1,10), 
#'                            pschoolid=c(11,12), pgroupid=c(13,14),
#'                            prespid=c(15,16), pratings=c(18,27)))
#' (klas2.rat = readratdatafixed(filenames[9], 
#'                             pnames=c(10,20), 
#'                             pschoolid=c(1,2), pgroupid=c(3,4),
#'                             prespid=c(5,6), pratings=c(21,32)))
#' 
#' @importFrom plyr rbind.fill
#' 
#' @export
readratdatafixed <- function(filename, pnames = c(0, 0), pschoolid = c(1, 
    2), pgroupid = c(3, 4), prespid = c(5, 6), pratings = c(8, 17), rowsEQassessors = T) {
  
    readgroupdata <- function(group_data, groupnr = 1) {
        nrresp = length(group_data)
        if (!all(pnames == 0)) 
            {
              resplabel = substr(group_data, pnames[1], pnames[2])
              resplabel = sub("\\s+$", "", resplabel)
            }  # delete trailing whitespace
        if (!all(pschoolid == 0)) 
            schoolid = as.numeric(substr(group_data, pschoolid[1], pschoolid[2]))
        else schoolid=1
        if (!all(pgroupid == 0)) 
            groupid = as.numeric(substr(group_data, pgroupid[1], pgroupid[2]))
        if (!all(prespid == 0)) 
            respid = as.numeric(substr(group_data, prespid[1], prespid[2]))
        if (!all(pratings == 0)) {
            lr = min(pratings[2], nchar(group_data[1]))
            if (lr <= 2) stop(paste("Unexpected empty line in ", groupnr, "th group.", sep=''))
            ratings = substring(group_data, pratings[1], lr)
            nrratings = lr - pratings[1] + 1
            ratingnames=sprintf("r%02.0f", 1:nrratings)
            r = unlist(strsplit(ratings, split = NULL))
            r[r == "0"] = NA
            if (rowsEQassessors) {
                rmat = t(suppressWarnings(matrix(as.numeric(r), nrow = nrratings, 
                  ncol = nrresp)))
            } else {
                rmat = suppressWarnings(matrix(as.numeric(r), nrow = nrratings, 
                  ncol = nrresp))
            }
        }
        
        DF = as.data.frame(cbind(schoolid, groupid, respid, rmat))
        colnames(DF) <- c("schoolid", "groupid", "respid", ratingnames)
        if (all(pnames > 0)) {
            DF = cbind(resplabel, DF)
        }
        
        return(DF)
    }
    
    
    all_data = readLines(filename)
    nlines = length(all_data)
    
    emptylines = which(sapply(all_data, nchar) == 0)
    nrc = length(emptylines)
    
    start = 1
    nrgroup = 0
    if (nrc == 0) {
        group_data = all_data
        DF = readgroupdata(group_data, 1)
    } else {
        # i=1
        for (i in 1:nrc) {
            group_data = all_data[start:(emptylines[i] - 1)]
            nrgroup = nrgroup + 1
            if (nrgroup == 1) {
                DF = readgroupdata(group_data, nrgroup)
            } else {
                temp = readgroupdata(group_data, nrgroup)
                DF = rbind.fill(DF, temp)
            }
            start = emptylines[i] + 1
        }
        nrgroup = nrgroup + 1
        group_data = all_data[start:nlines]
        temp = readgroupdata(group_data, nrgroup)
        DF = rbind.fill(DF, temp)
    }
    return(DF)
}






