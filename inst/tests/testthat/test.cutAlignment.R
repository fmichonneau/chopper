
context("cutAlignment")

algPhylip <- file("testAlignment.phy", "w")
cat("3 15",
    " aaaaatttttccccc",
    " tttttcccccggggg",
    " ccccc-----ttttt",
    sep="\n", file=algPhylip)
close(algPhylip)

partF <- file("testPart.part", "w")
cat("DNA, part1=1-5",
    "DNA, part2=6-12",
    "DNA, part3=13-15",
    sep="\n", file=partF)
close(partF)

part2F <- file("testPart2.part", "w")
cat("DNA, part=1-15", sep="\n", file=part2F)
close(part2F)

part1File <- file("partition1.phy", "w")
cat("3 5",
    " aaaaa",
    " ttttt",
    " ccccc",
    sep="\n", file=part1File)
close(part1File)

part2File <- file("partition2.phy", "w")
cat("3 7",
    " tttttcc",
    " cccccgg",
    " -----tt",
    sep="\n", file=part2File)
close(part2File)

part3File <- file("partition3.phy", "w")
cat("3 3",
    " ccc",
    " ggg",
    " ttt",
    sep="\n", file=part3File)
close(part3File)

cutAlignment(algfile="testAlignment.phy", partfile="testPart.part",
             formatin="sequential", format="sequential", colsep="")

test_that("cutAlignment creates files correctly", {
    expect_true(file.exists("testAlignment_part1.phy"))
    expect_true(file.exists("testAlignment_part2.phy"))
    expect_true(file.exists("testAlignment_part3.phy"))
})

test_that("cutAlignment cuts where it is supposed to when partitions are specified", {
    part1Expect <- readLines("partition1.phy")
    part2Expect <- readLines("partition2.phy")
    part3Expect <- readLines("partition3.phy")
    part1Gen <- readLines("testAlignment_part1.phy")
    part2Gen <- readLines("testAlignment_part2.phy")
    part3Gen <- readLines("testAlignment_part3.phy")
    expect_identical(part1Gen, part1Expect)
    expect_identical(part2Gen, part2Expect)
    expect_identical(part3Gen, part3Expect)
})

cutAlignment(algfile="testAlignment.phy", partfile="testPart2.part",
             formatin="sequential", colsep="")

test_that("cutAlignment works with 1 single partition specified", {
    onePartGen <- readLines("testAlignment_part.phy")
    onePartExpected <- readLines("testAlignment.phy")
    expect_identical(onePartGen, onePartExpected)
})

lF <- list.files(pattern="phy$")
lP <- list.files(pattern="part$")
sapply(c(lF, lP), unlink)
