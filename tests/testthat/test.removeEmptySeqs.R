
context("removeEmptySeqs")

test_that("phylip gaps as -", {
    cat("3 6",
        "sp1   at-t-t",
        "sp2   gt----",
        "sp3   ------",
        sep="\n", file="testPhy.phy")
    resMade <- removeEmptySeqs("testPhy.phy", output="testPhy_red.phy",
                               formatin="sequential", gap="-")
    resOut <- ape::read.dna(file="testPhy_red.phy", format="sequential")
    expect_equal(dim(resOut), c(2, 6))
    expect_equal(dimnames(resOut)[[1]], c("sp1", "sp2"))
    expect_error(
        resMade <- removeEmptySeqs("testPhy.phy", formatin="seq",
                                   overwrite=FALSE, gap="-")
        )
    resMade <- removeEmptySeqs("testPhy.phy", formatin="seq",
                               overwrite=TRUE, gap="-")
    expect_equal(resMade, "sp3")    
    unlink("testPhy.phy")
    unlink("testPhy_red.phy")
})

test_that("phylip gaps as *", {
    cat("3 6",
        "sp1   at-t-t",
        "sp2   gt----",
        "sp3   ******",
        sep="\n", file="testPhy.phy")
    resMade <- removeEmptySeqs("testPhy.phy", output="testPhy_red.phy",
                               formatin="sequential", gap="*")
    resOut <- ape::read.dna(file="testPhy_red.phy", format="sequential")
    expect_equal(dim(resOut), c(2, 6))
    expect_equal(dimnames(resOut)[[1]], c("sp1", "sp2"))
    expect_error(
        resMade <- removeEmptySeqs("testPhy.phy", formatin="seq",
                                   overwrite=FALSE, gap="*")
        )
    resMade <- removeEmptySeqs("testPhy.phy", formatin="seq",
                               overwrite=TRUE, gap="*")
    expect_equal(resMade, "sp3")    
    unlink("testPhy.phy")
    unlink("testPhy_red.phy")
})

