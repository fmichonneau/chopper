
##' Split alignments into individual files.
##'
##' This function takes an alignment and put each sequence into its
##' individual file that is named the same as the name of the
##' sequence.  Using the option convertEnds requires that you can read
##' and write to /tmp. This function also overwrites the output (it
##' doesn't check that the file created don't already exist).
##' @title Split alignments. 
##' @param file path and file name of the alignment (all the seq need to be the same length) 
##' @param output folder where the individual sequences will be stored (no trailing /)
##' @param format format of the alignment, for now only fasta
##' @param colw width of columns, same as in ape:::write.dna
##' @param convertEnds should the extremities of the sequences be
##' converted to missing data? (default=FALSE)
##' @param checkAmbiguity should the presence of ambiguities in the
##' sequences be tested? (default=TRUE). If ambiguities are found,
##' returns a warning.
##' @param checkInternalGaps should the presence of internal gaps be
##' tested? (default=FALSE). If internal gaps are found, returns a
##' warning.
##' @param ... additional parameters to be passed to ape:::write.dna
##' @return TRUE if the function succeed, mostly used for its side
##' effect (create files with individual sequences).
##' @author Francois Michonneau
##' @export
splitAlignment <- function(file, output, format="fasta",
                           colw=10000, convertEnds=FALSE, checkAmbiguity=TRUE,
                           checkInternalGaps=FALSE, ...) {
    
    format <- match.arg(format)

    if (checkAmbiguity) {
        chk <- checkAmbiguity(file=file, format=format)
        if (length(chk) > 0) {
            warning("Your alignment has ambiguities.")
        }
    }
    if (checkInternalGaps) {
        chkGap <- checkInternalGaps(file=file, format=format)
        if (length(chkGap) > 0) {
            warning("Your alignment has internal gaps.")
        }
    }
    ## if (convertEnds) {
    ##     timeAlg <- format(Sys.time(), "%Y%m%d-%H%M%S")
    ##     tmpFnm <- file.path("/tmp", paste(timeAlg, "tmpAlignement.fas", sep="-"))
    ##     convertEndsToMissingAlg(file=file, format=format, output=tmpFnm)
    ##     file <- tmpFnm
    ## }
    alg <- read.dna(file=file, format=format)
    nbSeq <- dim(alg)[1]
    
    for (i in 1:nbSeq) {
        fnm <- file.path(output, dimnames(alg)[[1]][i])
        write.dna(alg[i, ], file=fnm, format="fasta", colsep="", colw=colw, ...)
        tmpSeq <- readLines(fnm)
        tmpSeq[1] <- gsub("\\s+", "", tmpSeq[1])
        cat(tmpSeq, file=fnm, sep="\n")
    }
    TRUE
}
