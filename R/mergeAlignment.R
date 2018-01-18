
##' Merge alignment files from identical markers.
##'
##' Given a list of alignment files, this function puts them together
##' in a single file. The alignmenents must be from the same marker to
##' be relevant. Additionally, if the alignment want to be preserved,
##' they must be of the same length. Otherwise, you'll need to re-run
##' an alignment program on the output file of this function. This
##' function tests that the specified \sQuote{output} file doesn't
##' exist. If it does, the function stops with an error.
##' @title Merge alignment files.
##' @param listFiles a list of fasta files to be put in the same
##' file. The path should be omitted and specified with the argument
##' \sQuote{seqFolder}.
##' @param output the path and name of the output file.
##' @param seqFolder the folder that holds the fasta files specified by \sQuote{listFiles}.
##' @return TRUE if the function succeeded. This function is mostly
##' used for its side effect of merging different sequences/alignments.
##' @author Francois Michonneau
##' @export
mergeAlignment <- function(listFiles, output, seqFolder) {

    stopifnot(!file.exists(output))
    owd <- getwd()
    on.exit(setwd(owd))
    setwd(seqFolder)
    for (i in 1:length(listFiles)) {
        tmpXX <- scan(file=listFiles[i], what="character", sep="\n",
                      quiet = TRUE)
        cat(tmpXX, sep="\n", file=output, append=TRUE)
    }
    TRUE
}
