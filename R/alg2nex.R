
##' Converts an alignment from the PHYLIP or FASTA format to the NEXUS
##' format.
##'
##' This function only takes alignments (all sequences must be the
##' same length). It uses a slightly modified version of ape's
##' \code{write.nexus.data}. With this function it is also possible to
##' append partition information at the end of the alignment. The
##' partition information has to be written in the NEXUS format and be
##' provided in a separate file.
##' 
##' @title Converts from PHYLIP or FASTA format to NEXUS format.
##' @param file file in phylip format (\code{format="sequential"}) or
##' in fasta format (\code{format="fasta"})
##' @param format format argument to be passed to \code{ape::read.dna}
##' @param interleaved interleaved argument to be passed to
##' \code{ape::read.dna}
##' @param gap character for gaps (to be passed to write.nexus.data)
##' @param missing character for msising data (to be passed to
##' write.nexus.data)
##' @param partition.file if \code{NULL} nothing happens, if it's the
##' path to a valid file is given this file is appended to the
##' alignement. Particularly useful to include partition information
##' in the NEXUS file.
##' @return nothing, used for its side of generating a NEXUS file
##' @author Francois Michonneau
##' @seealso ape::write.nexus.data
##' @include seq.write.nexus.data.R
##' @export
alg2nex <- function(file, format = "sequential", interleaved = FALSE, gap = "-",
                    missing = "?", partition.file = NULL) {
    alg <- read.dna(file = file, format = format, as.character = TRUE,
                    as.matrix=TRUE)
    nfnm <- gsub("(\\.?.+)\\.(.+)$", "\\1.nex", file)
    nalg <- apply(alg, 1, function(x) paste(x, sep = "", collapse = ""))
    seq.write.nexus.data(nalg, file = nfnm, interleaved = interleaved, gap = gap,
                         missing = missing)
    if (! is.null(partition.file)) {
        if (file.exists(partition.file)) {
            outFile <- file(nfnm, "a")
            part <- readLines(partition.file)
            writeLines(part, outFile)
            close(outFile)
        }
    }
}
