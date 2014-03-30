
##' Check for internal gaps.
##'
##' This function takes a DNA alignment and looks for internal gaps
##' (gaps in the middle of a sequence). For protein-coding DNA
##' sequences, these indicate that the original file (e.g., trace
##' files) need to be inspected closely.
##' @title Check for internal gaps in a DNA sequence.
##' @param file path and file name for a DNA alignment.
##' @param format format of the DNA alignment.
##' @param quiet if FALSE, outputs as messages the list of gaps found
##' in the sequences.
##' @param ... additional arguments to be passed to ape:::read.dna
##' @return If there are no internal gaps, the function returns a
##' zero-length integer vector. Otherwise, if quiet=TUE, the function
##' returns a named vector that indicates the position in the
##' alignment of the internal gaps for each sequence that has one or
##' more internal gaps, if quiet=FALSE, this vector is returned
##' invisibly in addition of the messages.
##' @author Francois Michonneau
##' @export
checkInternalGaps <- function(file, format="fasta", quiet=TRUE, ...) {
    alg <- read.dna(file=file, format=format, as.character=TRUE, as.matrix=TRUE,
                    ...)
    tmpAlg <- apply(alg, 1, function(x) paste0(x, collapse=""))
    listGap <- sapply(tmpAlg, function(x) gregexpr("[actgn]-+[actgn]", x, ignore.case=TRUE))
    hasGap <- sapply(listGap, function(x) x[1] != -1)
    locGap <- lapply(listGap[hasGap], function(x) x[1:length(x)]+1)
    if (any(hasGap)) {
        if (!quiet) {
            message("There are internal gaps in the sequences.")
            for (i in 1:length(locGap)) {
                message(names(locGap)[i], " has internal gaps at position(s): ")
                message("  ", paste(locGap[[i]], collapse=", "))
            }
            invisible(locGap)
        }
        else locGap
    }
    else integer(0)
}
