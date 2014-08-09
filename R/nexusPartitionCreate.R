##' Given a PHYLIP file, this function returns a NEXUS formatted
##' partition file that is split by codon position.
##'
##' By default the number of partitions is determined by how many
##' names are provided.
##'
##' The file used (supplied with \code{alg}) must be a PHYLIP
##' formatted file: the first line must contain the number of
##' sequences, followed by the length of the sequences.
##'
##' @title Create a NEXUS-formatted partition file
##' @param alg The path to a PHYLIP formatted file
##' @param file.out The file name and path to the output file
##' @param part.names The names of the partitions used, the length of
##' this arguments determines how many partitions are generated. By
##' default 3, making one partition per codon position.
##' @param overwrite If a file with the same name as \code{file.out}
##' exists, should it be overwritten? (default \code{FALSE}.
##' @return a named logical vector indicating whether the file was
##' created.
##' @export
##' @author Francois Michonneau
nexusPartitionCreate <- function(alg, file.out="nexus_partition",
                                 part.names=c("p1", "p2", "p3"),
                                 overwrite=FALSE) {
    if (file.exists(file.out) && !overwrite) stop(file.out, " already exists.")
    alg <- readLines(alg, n=1)
    lSeq <- unlist(strsplit(alg, " "))[2]
    res <- paste0("  charset ", part.names, " = ",
                  1:length(part.names), "-", lSeq, "\\",
                  length(part.names), ";")
    cat("begin assumptions;",
        res,
        "end;",
        file=file.out, sep="\n")
    setNames(file.exists(file.out), file.out)
}

## TODO -- write tests.
