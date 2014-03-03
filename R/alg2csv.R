##' Converts an alignment into a CSV file.
##'
##' This function converts an alignement (in FASTA format by default,
##' but accepts all formats accepted by ape:::read.dna) into a
##' 2-column CSV file, where the first column is the sequence name,
##' and the second column the actual sequence. The sequences are
##' ordered alphabetically, and converted to upper case.
##' @title algToCsv (alignemnt to CSV)
##' @param file path and file name for a DNA alignement.
##' @param output  path and file name of the CSV file which will contain the alignement.
##' @param format format of the alignement specified by the \sQuote{file} argument.
##' @return TRUE if the function succeeded, and writes the
##' \sQuote{output} file where specified.
##' @author Francois Michonneau
alg2Csv <- function(file, output, format="fasta") {
    alg <- read.dna(file=file, format=format, as.character=TRUE)
    if (file.exists(output)) stop(output, " already exists.")
    for (i in order(dimnames(alg)[[1]])) {
        seq <- paste(alg[i, ], collapse="")
        seq <- toupper(seq)
        cat("\"", dimnames(alg)[[1]][i], "\"", ",", sep="", file=output, append=TRUE)
        cat("\"", seq, "\"", sep="", file=output, append=TRUE)
        cat("\n", file=output, append=TRUE)
    }
    TRUE
}
