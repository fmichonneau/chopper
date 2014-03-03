\name{cutAlignment}
\alias{cutAlignment}
\title{Cut an alignment into individual partitions.}
\usage{
cutAlignment(algfile, partfile, formatin = "fasta", ...)
}
\arguments{
  \item{algfile}{file name of the alignment to cut}

  \item{partfile}{file name of the partition file}

  \item{formatin}{format of the DNA alignment (to be passed
  to ape:::read.dna)}

  \item{...}{additional arguments controlling the formating
  of the output (to be passed to ape:::write.dna)}
}
\value{
TRUE if the function worked, used for its side effect of
creating individual partition files.
}
\description{
Given an alignment and a partition file, takes the
alignment and cuts it into individual partitions (opposite
of mergeSeq)
}
\details{
At this time, only takes a RAxML formatted partition file.
}
\author{
Francois Michonneau
}

