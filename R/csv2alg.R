##' Converts a two-column CSV file (first column: label, second column:
##' sequence) into a FASTA file.
##'
##' ss
##'
##' @title Converts CSV to FASTA
##' @param csv_file path to a CSV file
##' @param output file to write the output
##' @return invisibly the file name created
##' @export
##' @importFrom readr read_csv
##' @importFrom glue glue
csv2fasta <- function(csv_file, output) {
    seqs <- readr::read_csv(csv_file, col_types="cc")
    res <- character(nrow(seqs)*2)
    seq_nm_i <- seq(1, to=nrow(seqs)*2, by=2)
    seq_i <- seq_nm_i + 1
    res[seq_nm_i] <- glue::glue(">{dplyr::pull(seqs[1])}")
    res[seq_i] <- dplyr::pull(seqs[2])
    cat(res, file=output, sep = "\n")
    invisible(output)
}
