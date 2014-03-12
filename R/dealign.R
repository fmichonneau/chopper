
##' De-align an alignment
##'
##' Removed all gaps in an alignment
##' @title Dealign an alignment
##' @param file input file (in FASTA)
##' @param output output file (in FASTA)
##' @return TRUE if function worked, nothing otherwise. Used for its
##' side effect of creating a file.
##' @author Francois Michonneau
##' @export
dealign <- function(file, output) {
    
    fContent <- scan(file=file, what="character", sep="\n")

    beg <- grep("^>", fContent)
    end <- c(beg[2:length(beg)]-1, length(fContent))
    cat(character(0), file=output)
    for (i in 1:length(beg)) {
        cat(fContent[beg[i]], "\n", file=output, sep="", append=TRUE)
        concSeq <- paste(fContent[(beg[i]+1):end[i]], collapse="", sep="")
        concSeq <- gsub("-", "", concSeq)
        cat(concSeq, "\n", file=output, sep="", append=TRUE)
    }
    TRUE
}
