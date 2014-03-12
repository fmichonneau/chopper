
##' Given an alignment and a partition file, takes the alignment and
##' cuts it into individual partitions (opposite of mergeSeq)
##'
##' At this time, only takes a RAxML formatted partition file.
##' @title Cut an alignment into individual partitions.
##' @param algfile file name of the alignment to cut
##' @param partfile file name of the partition file
##' @param formatin format of the DNA alignment (to be passed to ape:::read.dna)
##' @param ... additional arguments controlling the formating of the
##' output (to be passed to ape:::write.dna)
##' @return TRUE if the function worked, used for its side effect of
##' creating individual partition files.
##' @author Francois Michonneau
##' @export
cutAlignment <- function(algfile, partfile, formatin="fasta", ...) {

    pInfo <- raxmlPartitionInfo(partfile)

    pNm <- pInfo$locusName
    pBeg <- pInfo$locusStart
    pEnd <- pInfo$locusEnd

    alg <- read.dna(file=algfile, format=formatin, as.matrix=TRUE)

    for (i in 1:length(pBeg)) {
        tmpAlg <- alg[, pBeg[i]:pEnd[i]]
        ext <- gsub("(.+)\\.([a-zA-Z]{3,}$)", paste("\\1_", pNm[i], ".\\2", sep=""), algfile)
        write.dna(tmpAlg, file=ext, ...)
    }
    TRUE
}
