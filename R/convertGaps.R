
##' Convert gap characters in an alignemnt
##'
##' This function is intended to replace one type of gap character
##' (e.g., \sQuote{?}) into another (e.g., \sQuote{-}).
##' @title Convert gap characters
##' @param file path and file name of a DNA alignment
##' @param output path and file name for the modified file
##' @param formatin format of the input file (to be passed to
##' \code{\link[ape]{read.dna}}).
##' @param formatout format for the output file (to be passed to
##' \code{\link[ape]{write.dna}}, by default the same as
##' \code{formatin}).
##' @param colsep argument to be passed to
##' \code{\link[ape]{write.dna}}, character used to separate columns
##' in DNA alignment (\code{ape}'s default is a single space, here we
##' override this to no character, i.e. \code{colsep=""}).
##' @param from character to be changed in alignment (default \dQuote{?})
##' @param to new character to use in alignment (default \dQuote{-})
##' @param overwrite if no output file is specified, and
##' \code{overwrite} is TRUE, then the original file is overwritten.
##' @param ... additional arguments to be passed to
##' \code{\link[ape]{write.dna}}.
##' @return the name of the output file (invisibly).
##' @author Francois Michonneau
##' @export
convertGaps <- function(file, output, formatin="sequential", formatout=formatin,
                        colsep="", from="?", to="-", overwrite=FALSE, ...) {
    if (missing(output) && overwrite)  output <- file
    alg <- ape::read.dna(file=file, format=formatin, as.character=TRUE, as.matrix=TRUE)
    newalg <- apply(alg, 2, function(x) {
        x[x == from] <- to
        x
    })
    ape::write.dna(newalg, file=output, formatout, colsep="")
    invisible(output)
}

## TODO -- write tests
