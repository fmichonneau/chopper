##' Calculates the number of base pairs in each sequence of an alignment.
##'
##' The argument \code{gap} should be a valid regular expression whose
##' value is substituted (by \code{gsub}) into empty characters. The
##' remaining is the base pairs making up the sequences.
##'
##' @title Number of bases
##' @param file A file with DNA sequences
##' @param gap  The character(s) used as gaps in the file
##' @param format The file format of the alignment
##' @param ... Additional parameters to be passed to \code{\link[ape]{read.dna}}
##' @return a named vector indicating the number of non-gap characters found in each sequence.
##' @author Francois Michonneau
##' @export
nbase <- function(file, gap="-", format="fasta", ...) {
    alg <- ape::read.dna(file=file, format=format, as.character=TRUE, ...)
    lgt <- apply(alg, 1, function(x) {
        .seq <- gsub(gap, "", paste0(x,collapse=""))
        nchar(.seq)
    })
    lgt
}
