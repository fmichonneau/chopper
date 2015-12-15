##' Remove the comments in the titles of FASTA files (everything that
##' is after \sQuote{/}).
##'
##' @title Remove comments from FASTA files
##' @param file The file name of the alignment
##' @param output If \code{NULL} (default), the orignal file is
##'     overwritten, otherwise a file name for the output
##' @param ... Additional arguments to be passed to
##'     \code{\link[ape]{read.dna}}.
##' @return \code{TRUE}. Function used for its side effect.
##' @author Francois Michonneau
##' @export
removeFastaComments <- function(file, output = NULL, ...) {

    if (!file.exists(file))
        stop(file, " doesn't exist.")

    alg <- ape::read.dna(file = file, format = "fasta", as.matrix = TRUE, ...)
    dimnames(alg)[[1]] <- gsub("\\s?/.+$", "", dimnames(alg)[[1]])

    if (is.null(output)) output <- file

    ape::write.dna(alg, file = output, format = "fasta", colsep = "", colw = 10000)
    TRUE
}
