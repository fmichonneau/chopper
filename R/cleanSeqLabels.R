
##' This function cleans up the sequence labels in an object created
##' with ape::read.dna to remove illegal characters for a given
##' phylogenetic software or according to a regular expression
##' provided by the user.
##'
##' Given an alignment created by ape::read.dna, this function
##' replaces (1) all characters not supported by a given software
##' (only RAxML provided at this time), or (2) all characters that
##' match a regular expression provided by the user. Note that, the
##' software argument is ignored by the function if a pattern in
##' provided.
##'
##' The function also adds an attribute to the alignment object
##' \code{oldnames} to keep the original labels.
##'
##' Here is the list of regular expression used for the different software:
##' \describe{
##' \item{RAxML}{\code{":|,|\\(|\\)|;|\\[|\\]|\\'|\\s|\t"}}
##' }
##' @title cleanSeqLabels
##' @param alg an object created by the function ape::read.dna
##' @param pattern a regular expression that indicates the characters
##' to replace (default NULL). Leave it to NULL if you are using one
##' of the supported software.
##' @param software the software for which you are creating your
##' alignment, (pre-loaded pattern). This argument is ignored if
##' \code{pattern} is provided.
##' @param replaceWith the character string to be used to replace the
##' illegal characters.
##' @return An alignment file
##' @examples
##' \dontrun{
##' cat("5 5",
##' "A:B,C  AAAAA",
##' "(D);E  TTTTT",
##' "F[G]   CCCCC",
##' "H'''   GGGGG",
##' "IIJ    TTTTT", file="/tmp/testAlg.phy", sep="\n")
##'
##' exDNA <- read.dna(file="/tmp/testAlg.phy", format="seq")
##' dimnames(exDNA)[[1]][3] <- "F \t [G]"
##' dimnames(exDNA)[[1]][5] <- "I I' J\""
##' exDNA <- cleanSeqLabels(exDNA, software="RAxML")
##' attr(exDNA, "oldnames")
##' }
##' @export
##' @author Francois Michonneau
cleanSeqLabels <- function(alg, pattern=NULL, software="RAxML",
                           replaceWith="") {
    if (is.null(pattern)) {
        software <- match.arg(software)
        if (identical(software, "RAxML")) {
            pattern <- ":|,|\\(|\\)|;|\\[|\\]|\\'|\\s|\t"
        }
    }
    oldNm <- dimnames(alg)
    newNm <- gsub(pattern, replaceWith, oldNm[[1]])
    dimnames(alg)[[1]] <- newNm
    attributes(alg)$oldnames <- oldNm
    alg
}
