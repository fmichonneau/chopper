
##' Check for ambiguities or other characters that are not allowed in
##' haplotype sequences.
##'
##' This function takes a DNA alignment and looks for characters in
##' the sequence that are not A, C, T, G, -, ? (and possibly N). It
##' ignores case.
##' @title Check for ambiguities in a DNA sequence
##' @param file path and file name for a DNA alignment.
##' @param format format of the DNA alignment
##' @param Namb should Ns be considered ambiguities?
##' @param quiet if FALSE, outputs as messages the ambiguities found
##' @param ... additional arguments to be passed to ape:::read.dna
##' @return If there are no ambiguities, the function returns a
##' zero-length integer vector. Otherwise, if quiet=TRUE, the function
##' returns a named vector that indicates the position in the
##' alignment of the ambiguities for each sequence that has one or
##' more ambiguities; if quiet=FALSE, this vector is returned
##' invisibly in addition of the messages.
##' @author Francois Michonneau
checkAmbiguity <- function(file, format="fasta", quiet=FALSE, Namb=TRUE, ...) {
    alg <- read.dna(file=file, format=format, as.character=TRUE, as.matrix=TRUE,
                    ...)
    if(!Namb) {
        ambChar <- c("a", "c", "t", "g", "n", "-", "?", "A", "C", "T", "G", "N")
    }
    else {
        ambChar <- c("a", "c", "t", "g", "-", "?", "A", "C", "T", "G")
    }
    isAmbiguity <- apply(alg, 1, function(x) which(!x %in% ambChar))
    if (length(isAmbiguity)) {
        hasAmbiguity <- sapply(isAmbiguity, function(x) length(x) > 0)
        isAmbiguity <- isAmbiguity[hasAmbiguity]
        if (!quiet) {
            message("There are ambiguities.")
            for(i in 1:length(isAmbiguity)) {
                message(names(isAmbiguity)[i])
                for (j in 1:length(isAmbiguity[[i]])) {
                    message("  ", isAmbiguity[[i]][j], ": ", alg[names(isAmbiguity)[i], isAmbiguity[[i]]][j])
                }
            }
            invisible(isAmbiguity)
        }
        else isAmbiguity   
    }
    else integer(0)
}
