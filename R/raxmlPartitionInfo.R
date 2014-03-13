##' This function takes a RAxML partition file and returns information
##' about it as a list.
##'
##' Currently, this function ignores the type of data of each partition,
##' and does not support the codon notation (e.g., 1-300\3).
##' @title Information from a RAxML partition file
##' @param partfile Path of a RAxML partition file
##' @return A list that includes the elements:
##' \describe{
##' \item{\code{file}}{the partition file name}
##' \item{\code{locusName}}{a vector of character indicating the name of each partition}
##' \item{\code{locusStart}}{a vector of numeric indicating the first position in the alignment for the locus}
##' \item{\code{locusEnd}}{a vector of numeric indicating the last position in the alignement of the locus}}
##' @export
##' @author Francois Michonneau
raxmlPartitionInfo <- function(partfile) {
    pfile <- scan(file=partfile, what="character", sep="\n", quiet=TRUE)
    pfile <- sapply(pfile, function(x) gsub("\\s?", "", x))
    pLoc <- sapply(strsplit(pfile, ","), function(x) x[2])
    pLoc <- strsplit(pLoc, "=")
    pNm <- sapply(pLoc, function(x) x[1])
    pRange <- sapply(pLoc, function(x) x[2])
    pRange <- strsplit(pRange, "-")
    pBeg <- lapply(pRange, function(x) x[1])
    pBeg <- as.integer(pBeg)
    pEnd <- lapply(pRange, function(x) x[2])
    pEnd <- as.integer(pEnd)

    list(file=partfile, locusName=pNm, locusStart=pBeg, locusEnd=pEnd)
}
