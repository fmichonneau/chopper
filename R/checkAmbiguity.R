
##' Check for ambiguities or other characters that are not allowed in
##' haplotypes.
##'
##' This function takes a DNA alignment and looks for characters in
##' the sequence that are not A, C, T, G, -, ? (and optionally N). It
##' ignores case.
##' @title Check for ambiguities in a DNA sequence
##' @param file path and file name for a DNA alignment (character)
##' @param format format of the DNA alignment (passed to
##' \code{\link[ape]{read.dna}})
##' @param Namb should Ns be considered ambiguities? (logical)
##' @param quiet if \code{FALSE}, outputs as messages the ambiguities
##' found (logical)
##' @param simplify if \code{TRUE}, the returned object is reduced to
##' only include the sequences that contain ambiguities. If
##' \code{FALSE}, the returned object has the same length as the
##' number of sequences in the alignment.
##' @param ... additional arguments to be passed to ape:::read.dna
##' @return If there are no ambiguities, the function returns a
##' zero-length integer vector. Otherwise, it returns a named list
##' which includes for each sequence, the position in the alignment of
##' the ambiguities for each sequence that has one or more ambiguities
##' (if \code{simplify=TRUE}), and is named using the ambiguity. if
##' quiet=FALSE, this vector is returned invisibly in addition of the
##' @author Francois Michonneau
##' @export
checkAmbiguity <- function(file, format="fasta", quiet=FALSE, Namb=TRUE,
                           simplify=TRUE, ...) {
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
        lstAmbiguity <- sapply(1:nrow(alg), function(x) alg[x, isAmbiguity[[x]]])
        if (simplify) {
            isAmbiguity <- isAmbiguity[hasAmbiguity]
            lstAmbiguity <- lstAmbiguity[hasAmbiguity]
        }
        res <- mapply(function(isAmb, lstAmb) { names(isAmb) <- lstAmb; isAmb },
                      isAmbiguity, lstAmbiguity, SIMPLIFY=FALSE)
        if (!quiet) {
            message("There are ambiguities.")
            if (simplify) {
                resTmp <- res
            }
            else {
                resTmp <- res[sapply(res, function(x) length(x) > 0)]
            }
            for(i in 1:length(resTmp)) {
                message(names(resTmp)[i])
                for (j in 1:length(resTmp[[i]])) {
                    message("  ", resTmp[[i]][j], ": ", names(resTmp[[i]][j]))
                }
            }
        }
        invisible(res)
    }
    else integer(0)
}
