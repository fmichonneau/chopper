
##' Removes empty sequences from an alignment
##'
##' This function takes an alignemnt, and remove sequences that only
##' contains gaps. This function can read the same type of alignment
##' files as ape:::read.dna, and output the same formats as
##' ape:::write.dna
##' @title Remove empty sequences 
##' @param file path and file name for a DNA alignment
##' @param output path and file name of the alignment with the missing
##' data removed. If missing, the original file is overwritten, with
##' an a posteriori warning, unless quiet=TRUE.
##' @param formatin format of the alignment to check (to be passed to ape:::read.dna)
##' @param formatout format of the alignemnt to write (to be passed to ape:::write.dna)
##' @param quiet If FALSE, no warning is issued to indicate that the
##' original file has been overwritten.
##' @param gap character in the sequence to be considered as missing data
##' @param ... additional arguments to be passed to ape:::read.dna.
##' @return TRUE if function succeedeed, but mostly used for its side
##' effect of creating a new alignment file.
##' @author Francois Michonneau
##' @export
removeEmptySeqs <- function(file, output, formatin="fasta", formatout="fasta",
                            quiet=FALSE, gap="?", ...) {
    
    if (missing(output)) {
        output <- file
        if (!quiet) warning("no output precised, original file overwritten.")
    }
    gapchar <- ifelse(gap == "?", "\\?", "-") # TODO need better
    alg <- read.dna(file, format=formatin, as.character=TRUE, as.matrix=TRUE, ...)     
    nMissing <- apply(alg, 1, function(x) length(grep(gapchar, x)))
    whichNotMissing <- sapply(nMissing, function(x) x != dim(alg)[2])
    alg <- alg[whichNotMissing, ]
    write.dna(alg, file=output, format=formatout, colsep="", colw=10000)
    invisible(names(whichNotMissing[!whichNotMissing]))
}
