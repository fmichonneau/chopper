
##' Split alignments into individual files.
##'
##' This function takes an alignment and put each sequence into its
##' individual file that is named the same as the name of the
##' sequence. This function also overwrites the output (it doesn't
##' check that the file created don't already exist).
##'
##' @title Split alignments.
##' @param file path and file name of the alignment (all the seq need
##'     to be the same length)
##' @param output folder where the individual sequences will be stored
##' @param split_by If \code{NULL} (default), each sequence will be a
##'     in a separate file. Otherwise, a data frame with at least 2
##'     columns named \sQuote{groups} and \sQuote{sequences}:
##'     sequences identified in the \sQuote{sequences} column, sharing
##'     the same values in the \sQuote{groups} column will be written
##'     to files.
##' @param format format of the alignment, for now only fasta
##' @param colw width of columns, same as in
##'     \code{\link[ape]{write.dna}}
##' @param checkAmbiguity should the presence of ambiguities in the
##'     sequences be tested? (default=TRUE). If ambiguities are found,
##'     returns a warning.
##' @param checkInternalGaps should the presence of internal gaps be
##'     tested? (default=FALSE). If internal gaps are found, returns a
##'     warning.
##' @param ... additional parameters to be passed to ape:::write.dna
##' @return TRUE if the function succeed, mostly used for its side
##'     effect (create files with individual sequences).
##' @author Francois Michonneau
##' @importFrom ape read.dna
##' @importFrom ape write.dna
##' @export
splitAlignment <- function(file, output,
                           split_by = NULL,
                           format="fasta",
                           colw=10000,
                           checkAmbiguity=TRUE,
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

    alg <- ape::read.dna(file=file, format=format)
    nbSeq <- dim(alg)[1]
    fnm <- file.path(output, dimnames(alg)[[1]])

    if (is.null(split_by)) {
        for (i in 1:nbSeq) {
            fnm_ <- fnm[i]
            ape::write.dna(alg[i, ], file=fnm_, format="fasta", colsep="", colw=colw, ...)
            tmpSeq <- readLines(fnm_)
            tmpSeq[1] <- gsub("\\s+", "", tmpSeq[1])
            cat(tmpSeq, file=fnm_, sep="\n")
        }
    } else {
        if (!inherits(split_by, "data.frame")) {
            stop(sQuote("split_by"), " must be a data frame.")
        }
        if (ncol(split_by) < 2) {
            stop(sQuote("split_by"), " must have at least 2 columns.")
        }
        if (! all(c("sequences", "groups") %in% names(split_by))) {
            stop(sQuote("split_by"), " must have columns named ",
                 sQuote("groups"), " and ", sQuote("sequences"))
        }
        grps <- split(split_by[["sequences"]], split_by[["groups"]])
        fnm <- file.path(output, names(grps))
        for (i in seq_along(grps)) {
            ape::write.dna(alg[na.omit(match(grps[[i]], dimnames(alg)[[1]])), ],
                           file = fnm[i],
                           format = "fasta", colsep = "", colw = colw, ...)
            tmp_seq <- readLines(fnm[i])
            tmp_seq[1] <- gsub("\\s+", "", tmp_seq[1])
            cat(tmp_seq, file = fnm[i], sep = "\n")
        }
    }
    TRUE
}
