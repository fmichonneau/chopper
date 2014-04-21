
##' Converts FASTA files to a relaxed PHYLIP format.
##'
##' Simple wrapper for ape::read.dna and ape::write.dna to facilitate
##' file conversion. The output file gets the same name as the input
##' file with the extension \dQuote{.phy}. The function only accepts
##' alignments (all sequences must be the same lengths). This function
##' also allows to remove some sequences from the alignment (using
##' their names).
##'
##' @title Converts FASTA files into PHYLIP format
##' @param file Character string indicating the FASTA file to convert.
##' @param format The output format for the file: \code{sequential} or
##' \code{interleaved}.
##' @param toDrop Character vector indicating the names of the
##' sequences to remove.
##' @param overwrite If there is already a file with the same names as
##' the output file in the working directory, should it be
##' overwritten? (logical, default \code{FALSE}).
##' @param ... Additional arguments to be passed to 
##' @return A logical vector indicating whether the file was created
##' named with the name of the file.
##' @seealso ape::write.dna
##' @author Francois Michonneau
fas2phy <- function(file, format="sequential", toDrop,
                    overwrite=FALSE, ...) {
    outfile <- gsub("\\.[a-z]+$", ".phy", file)
    if (!overwrite && file.exists(outfile))
        stop("A file named ", outfile, " already exists.")
    else {
        alg <- ape::read.dna(file=file, format="fasta", as.matrix=TRUE)
        if (!missing(toDrop)) {
            toRm <- match(toDrop, dimnames(alg)[[1]])
            if (length(toDrop) > 1 && length(toDrop) != length(toRm)) {
                stop("toDrop argument incorrectly formatted.")
            }
            alg <- alg[-toRm, ]
        }
        colWidth <- max(sapply(alg, length))
        ape::write.dna(alg, file=outfile, colsep="", format=format,
                       colw=colWidth, ...)
        return(setNames(file.exists(outfile), outfile))
    }
}
