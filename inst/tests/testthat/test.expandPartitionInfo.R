
context("expandPartitionInfo")

test_that("multi partitions with groups and codons", {
    cat("DNA, p1=1-10\\3,2-10\\3",
        "DNA, p2=3-10\\3,11-13,14-18\\3",
        "DNA, p3=19-23\\3,20-23\\3",
        "DNA, p4=24-26",
        sep="\n", file="partExample5.part")
    tmpPart <- raxmlPartitionInfo("partExample5.part")
    unlink("partExample5.part")
    infPart <- expandPartitionInfo(tmpPart)
    expPart <- list(
        p1 = c(1, 4, 7, 10, 2, 5, 8),
        p2 = c(3, 6, 9, 11, 12, 13, 14, 17),
        p3 = c(19, 22, 20, 23),
        p4 = c(24, 25, 26))
    expect_equal(expPart, infPart)
})
