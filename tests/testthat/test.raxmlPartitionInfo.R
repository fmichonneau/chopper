
context("raxmlPartitionInfo")

test_that("single partition", {
    cat("DNA, part1 = 1-300", sep="\n",
        file="partExample1.part")
    tmpPart <- raxmlPartitionInfo("partExample1.part")
    unlink("partExample1.part")
    expPart <- list(part1 = list(c(begin = 1, end = 300, by = 1)))
    expect_equal(tmpPart, expPart)
})

test_that("single partition with codons", {
    cat("DNA, part1=1-300\\3", sep="\n",
        file="partExample1.part")
    tmpPart <- raxmlPartitionInfo("partExample1.part")
    unlink("partExample1.part")
    expPart <- list(part1 = list(c(begin=1, end=300, by=3)))
    expect_equal(tmpPart, expPart)
})

test_that("single partition with groups and codons", {
    cat("DNA, part1=1-300\\3,2-300\\3,3-300\\3", sep="\n",
        file="partExample1.part")
    tmpPart <- raxmlPartitionInfo("partExample1.part")
    unlink("partExample1.part")
    expPart <- list(part1 =
                    list(c(begin=1, end=300, by=3),
                         c(begin=2, end=300, by=3),
                         c(begin=3, end=300, by=3)))
    expect_equal(tmpPart, expPart)
})


test_that("multi partitions with weird spaces", {
    cat("DNA, part1=1-300", "DNA,part2 =301 - 500", "DNA , part3=501- 600",
        sep="\n", file="partExample2.part")    
    tmpPart <- raxmlPartitionInfo("partExample2.part")
    unlink("partExample2.part")
    expPart <- list(part1 = list(c(begin=1, end=300, by=1)),
                    part2 = list(c(begin=301, end=500, by=1)),
                    part3 = list(c(begin=501, end=600, by=1)))
    expect_equal(tmpPart, expPart)
})

test_that("multi partitions with groups", {
    cat("DNA, part1=1-300,401-500,651-700",
        "DNA, part2=301-350",
        "DNA, part3=351-400,501-650",
    sep="\n", file="partExample3.part")
    tmpPart <- raxmlPartitionInfo("partExample3.part")
    unlink("partExample3.part")
    expPart <- list(part1 =
                    list(c(begin=1, end=300, by=1),
                         c(begin=401, end=500, by=1),
                         c(begin=651, end=700, by=1)),
                    part2 =
                    list(c(begin=301, end=350, by=1)),
                    part3 =
                    list(c(begin=351, end=400, by=1),
                         c(begin=501, end=650, by=1)))
    expect_equal(tmpPart, expPart)
})

test_that("multi partitions with codons", {
    cat("DNA, part1=1-60\\3",
        "DNA, part2=2-60\\3",
        "DNA, part3=3-60\\3",
        sep="\n", file="partExample4.part")
    tmpPart <- raxmlPartitionInfo("partExample4.part")
    unlink("partExample4.part")
    expPart <- list(part1 = list(c(begin=1, end=60, by=3)),
                    part2 = list(c(begin=2, end=60, by=3)),
                    part3 = list(c(begin=3, end=60, by=3)))
    expect_equal(tmpPart, expPart)                 
})

test_that("multi partitions with groups and codons", {
    cat("DNA, p1=1-60\\3,2-60\\3",
        "DNA, p2=3-60\\3,61-100,101-105\\3",
        "DNA, p3=102-105\\3,103-105\\3",
        "DNA, p4=106-120",
        sep="\n", file="partExample5.part")
    tmpPart <- raxmlPartitionInfo("partExample5.part")
    unlink("partExample5.part")
    expPart <- list(p1 =
                    list(c(begin=1, end=60, by=3),
                         c(begin=2, end=60, by=3)),
                    p2 =
                    list(c(begin=3, end=60, by=3),
                         c(begin=61, end=100, by=1),
                         c(begin=101, end=105, by=3)),
                    p3 =
                    list(c(begin=102, end=105, by=3),
                         c(begin=103, end=105, by=3)),
                    p4 =
                    list(c(begin=106, end=120, by=1)))
    expect_equal(tmpPart, expPart)
})
