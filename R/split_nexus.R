##' Split NEXUS files containing multiple trees into individual files
##' Useful for debugging and finding trees that fail to be loaded correctly.
##' @param file path for a NEXUS file (at this time only NEXUS files without a preambule work)
##' @param output folder where all the resulting NEXUS files will be written
split_nexus <- function(file, output, ...) {
    fc <- readr::read_lines(file)
    if (!file.exists(output))
        dir.create(output, showWarnings = FALSE, recursive = TRUE)

    fc_tr <- grep("^\\s?TREE", fc, value = TRUE)
    res <- lapply(fc_tr, function(x) {
        paste("#NEXUS\n",
            "BEGIN TREES; \n",
            x, "\n",
            "END;")
    })
    lapply(seq_along(res), function(x)
        cat(res[[x]], file = file.path(output, paste0("tree-", sprintf("%04i", x), ".nex")))
        )
}

### test

if (FALSE) {

    res <- list.files("/tmp/test_split/", pattern = "nex$", full.names = TRUE) %>%
        purrr:::map(purrr::safely(rncl::read_nexus_phylo))


    err <- transpose(res) %>%
        .$error %>%
        map_lgl(is_null)
}
