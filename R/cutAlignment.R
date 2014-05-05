
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
##' @param formatin format of the DNA alignment (to be passed to ape:::read.dna)
##' @param ... additional arguments controlling the formating of the
##' output (to be passed to ape:::write.dna)
##' @return TRUE if the function worked, used for its side effect of
##' creating individual partition files.
##' @seealso \code{\link{write.dna}}
##' @author Francois Michonneau
##' @examples
##' \dontrun{
##' cutAlignment("fullAlignment.phy", partfile="partInfo.part",
##'              formatin="sequential", format="sequential", colsep="")
##' }
##' @export
cutAlignment <- function(algfile, partfile, formatin="fasta", ...) {

    pInfo <- seqManagement::raxmlPartitionInfo(partfile)
    pExp <-  seqManagement::expandPartitionInfo(pInfo)
    
    pNm <- names(pExp)

    alg <- ape::read.dna(file=algfile, format=formatin, as.matrix=TRUE)

    for (i in 1:length(pExp)) {
        tmpAlg <- alg[, pExp[[i]]]
        ext <- gsub("(.+)\\.([a-zA-Z]{1,}$)",
                    paste("\\1_", pNm[i], ".\\2", sep=""), algfile)
        ape::write.dna(tmpAlg, file=ext, ...)
    }
    TRUE
}
