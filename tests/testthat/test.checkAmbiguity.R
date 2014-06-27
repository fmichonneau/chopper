
context("checkAmbiguity")

algPhylip <- file("testAlignment.phy", "w")
cat("3 15",
    "seq1  aaaaatttttNcccc",
    "seq2  tttttcccccggggg",
    "seq3   ccccc-?---ttttt",
    sep="\n", file=algPhylip)
close(algPhylip)

algPhylipAmb <- file("testAlignmentAmb.phy", "w")
cat("3 15",
    "seq1  aaaaatttttccccc",
    "seq2  ttttKcccYcggggg",
    "seq3  ccccc-M---ttttt",
    sep="\n", file=algPhylipAmb)
close(algPhylipAmb)

test_that("works when no ambiguity", {
    expect_equal(checkAmbiguity(file="testAlignment.phy", format="sequential", Namb=FALSE), integer(0))
    expect_equal(checkAmbiguity(file="testAlignment.phy", format="sequential", simplify=FALSE, Namb=FALSE),
                 integer(0))
    expect_message(checkAmbiguity(file="testAlignment.phy", format="sequential", simplify=FALSE, Namb=TRUE),
                   "There are ambiguities.")
})

test_that("works with ambiguity", {
    expect_message(checkAmbiguity(file="testAlignmentAmb.phy", format="sequential"),
                   "There are ambiguities")
})

test_that("returns correctly formatted object (simplify TRUE)", {
    amb <- checkAmbiguity(file="testAlignmentAmb.phy", format="sequential", quiet=TRUE, simplify=TRUE)
    expect_equal(names(amb), c("seq2", "seq3"))
    expect_equal(length(amb), 2)
    expect_equal(amb[[1]], setNames(c(5, 9), c("k", "y")))
    expect_equal(amb[[2]], setNames(7, "m"))
})

test_that("returns correctly formatted object (simplify FALSE)", {
    amb <- checkAmbiguity(file="testAlignmentAmb.phy", format="sequential", quiet=TRUE, simplify=FALSE)
    expect_equal(names(amb), c("seq1", "seq2", "seq3"))
    expect_equal(length(amb), 3)
    emptyRes <- integer(0)
    attr(emptyRes, "names") <- character(0)
    expect_equal(amb[[1]], emptyRes)
    expect_equal(amb[[2]], setNames(c(5, 9), c("k", "y")))
    expect_equal(amb[[3]], setNames(7, "m"))
})


lF <- list.files(pattern="phy$")
sapply(lF, unlink)
