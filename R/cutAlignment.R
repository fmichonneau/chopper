
##' Given an alignment and a partition file, takes the alignment and
##' cuts it into individual partitions.
##'
##' At this time, only takes a RAxML formatted partition file. The
##' individual partition generated are named with the full alignment
##' followed by the partition names specified in the partition file
##' separated by an underscore. For each of the file created, the same
##' extension as the full alignment is used. For instance, if your
##' alignment file is named \code{fullAlignment.phy}, and your
##' partitions are named \code{part1}, \code{part2} and \code{part3},
##' this function will generate the files
##' \code{fullAlignment_part1.phy}, \code{fullAlignment_part2.phy},
##' and \code{fullAlignment_part3.phy}.
##' 
##' @title Cut an alignment into individual partitions.
##' @param algfile file name of the alignment to cut
##' @param partfile file name of the partition file
##' @param formatin format of the DNA alignment (to be passed to
##' ape:::read.dna)
##' @param formatout format of the DNA alignment to be returned (by
##' default same as \code{formatin}, can be any value accepted by
##' \code{\link[ape]{write.dna}})
##' @param colsep argument to be passed to
##' \code{\link[ape]{write.dna}}, character used to separate columns
##' in DNA alignment (\code{ape}'s default is a single space, here we
##' override this to no character, i.e. \code{colsep=""}).
##' @param ... additional arguments controlling the formating of the
##' output (to be passed to ape:::write.dna)
##' @return TRUE if the function worked, used for its side effect of
##' creating individual partition files.
##' @seealso \code{\link{write.dna}}
##' @author Francois Michonneau
##' @examples
##' \dontrun{
##' cutAlignment("fullAlignment.phy", partfile="partInfo.part",
##'              formatin="sequential", colsep="")
##' }
##' @export
cutAlignment <- function(algfile, partfile, formatin="fasta", formatout=formatin,
                         colsep="", ...) {

    pInfo <- seqManagement::raxmlPartitionInfo(partfile)
    pExp <-  seqManagement::expandPartitionInfo(pInfo)
    
    pNm <- names(pExp)

    alg <- ape::read.dna(file=algfile, format=formatin, as.matrix=TRUE)

    fNm <- character(length(pExp))
    for (i in 1:length(pExp)) {
        tmpAlg <- alg[, pExp[[i]]]
        fNm[i] <- gsub("(.+)\\.([a-zA-Z]{1,}$)",
                    paste("\\1_", pNm[i], ".\\2", sep=""), algfile)
        ape::write.dna(tmpAlg, file=fNm[i], format=formatout,
                       colsep=colsep, ...)
    }
    invisible(fNm)
}
