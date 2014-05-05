##' This function takes a RAxML partition file and returns information
##' about it as a list.
##'
##' Currently, this function ignores the type of data of each
##' partition, does not support the codon notation (e.g.,
##' \code{1-300\3}), and requires continuous partitions (e.g.,
##' \code{1-300} works, but \code{1-300,401-500} doesn't work).
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
    pfile <- readLines(partfile)
    pfile <- sapply(pfile, function(x) gsub("\\s?", "", x))
    pLoc <- gsub("^DNA,", "", pfile)
    pLoc <- strsplit(pLoc, "=")
    pNm <- sapply(pLoc, function(x) x[1])
    pRange <- sapply(pLoc, function(x) x[2])
    pRange <- strsplit(pRange, ",")
    pBeg <- lapply(pRange, function(rg) {
        tmpRg <- lapply(rg, function(x) {
            sRg <- unlist(strsplit(x, "\\\\"))
            byRg <- ifelse(length(sRg) > 1, sRg[2], 1)
            curRg <- unlist(strsplit(sRg[1], "-"))
            res <- as.numeric(c(curRg, byRg))
            names(res) <- c("begin", "end", "by")
            res
        })
        tmpRg
    })    
    names(pBeg) <- pNm
    pBeg
}
