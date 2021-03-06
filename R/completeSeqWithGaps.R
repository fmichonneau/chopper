
##' Standardize the lengths of the sequences in a FASTA file by adding
##' gaps (or other characters) at the end.
##'
##' This function takes an alignment in which the sequences are of
##' different lengths (for instance, if 2 alignments are concatenated
##' manually), and standardizes the lengths by adding gaps at the end
##' of the sequences. If you want the output file to be aligned, you
##' need to make sure that the sequences are otherwise aligned (they
##' only differ by some gaps at their extremities).
##' @title Complete sequences with gaps.
##' @param file input file in the FASTA format or other format that
##' can be read by ape:::read.dna
##' @param output path and name file for the output.
##' @param format format fo the input file, to be passed to
##' ape:::read.dna
##' @param colsep a character used to separate the columns
##' @param gaps character to be as gap
##' @param ... further arguments to be passed to ape:::write.dna to
##' format the output file.
##' @return TRUE. This function is intended to be use for its side
##' effect which create a new alignment file with standardized
##' sequence lengths.
##' @author Francois Michonneau
##' @export
completeSeqWithGaps <- function(file, output, format = "fasta", colsep = "", gaps = "-", ...) {

    alg <- read.dna(file, format = "fasta", as.character = TRUE, as.matrix = FALSE)
    lSeq <- sapply(alg, length)
    maxLgth <- max(lSeq)
    toAdd <- maxLgth - lSeq
    gapsAdd <- sapply(toAdd, function(x) rep(gaps, x))
    for (i in 1:length(alg)) {
        alg[[i]] <- c(alg[[i]], gapsAdd[[i]])
    }
    write.dna(alg, file = output, format = format, colsep = colsep, ...)
    TRUE
}
