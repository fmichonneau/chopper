
##' Removes empty sequences from an alignment
##'
##' This function takes an alignment, and remove sequences that only
##' contains gaps. This function can read the same type of alignment
##' files as ape:::read.dna, and output the same formats as
##' ape:::write.dna
##' @title Remove empty sequences
##' @param file path and file name for a DNA alignment
##' @param output path and file name of the alignment with the missing
##' data removed. If missing, the original file is overwritten, with
##' an a posteriori warning, unless quiet=TRUE.
##' @param formatin format of the alignment to check (to be passed to
##' \code{\link[ape]{read.dna}})
##' @param formatout format of the alignemnt to write (to be passed to
##' \code{\link[ape]{write.dna}})
##' @param colsep argument to be passed to
##' \code{\link[ape]{write.dna}}, character used to separate columns
##' in DNA alignment (\code{ape}'s default is a single space, here we
##' override this to no character, i.e. \code{colsep=""}).
##' @param overwrite If no output file is specified and if overwrite is TRUE,
##' the original file is overwritten.
##' @param ... additional arguments to be passed to
##' \code{\link[ape]{write.dna}}.
##' @return TRUE if function succeedeed, but mostly used for its side
##' effect of creating a new alignment file.
##' @author Francois Michonneau
##' @export
removeEmptySeqs <- function(file, output, formatin="fasta", formatout=formatin,
                            colsep="", overwrite=FALSE, ...) {
    if (missing(output)) {
         if (overwrite)  output <- file
        else stop("Indicate output file or allow file to be overwritten.")
    }
    alg <- ape::read.dna(file, format=formatin, as.character=TRUE,
                         as.matrix=TRUE)
    ## an empty sequence is a sequence that only has one type of character
    ## and that is not a letter
    isNotEmpty <- apply(alg, 1, function(x) {
        table_x <- table(x)
        length(table_x) > 1 ||
            any(grepl("[a-z]", names(table_x)))
    })
    emptySeq <- dimnames(alg)[[1]][!isNotEmpty]
    alg <- alg[isNotEmpty, ]
    write.dna(alg, file=output, format=formatout, colsep=colsep, ...)
    invisible(emptySeq)
}
