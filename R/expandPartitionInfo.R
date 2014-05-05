
##' Expands information contained in a partitionInfo object to obtain
##' all base pair positions for a given alignment.
##'
##' The partition are expanded in the same order as they are placed in
##' the partition file.
##' @title expandPartitionInfo
##' @param partitionInfo a partitionInfo object
##' @return a list named with partition names, that contains all base
##' pair positions for each partition.
##' @examples
##' cat("DNA, p1 = 1-15\\3,2-15\\3",
##'     "DNA, p2 = 3-15\\3", sep="\n",
##'     file="testPartition.part")
##' pInfo <- raxmlPartitionInfo("testPartition.part")
##' expandPartitionInfo(pInfo)
##' unlink(testPartition.part)
##' @seealso raxmlPartitionInfo
##' @author Francois Michonneau
##' @export
expandPartitionInfo <- function(partitionInfo) {
    lapply(partitionInfo, function(p) {
        indSeq <- lapply(p, function(x) {
            x <- seq(x["begin"], x["end"], by=x["by"])
            unname(x)
        })
        unlist(indSeq)
    })    
}
