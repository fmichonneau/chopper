\name{alg2nex}
\alias{alg2nex}
\title{Converts from PHYLIP or FASTA format to NEXUS format.}
\usage{
alg2nex(file, format = "sequential", interleaved = FALSE, gap = "-",
  missing = "?", partition.file = NULL, ...)
}
\arguments{
  \item{file}{file in phylip format
  (\code{format="sequential"}) or in fasta format
  (\code{format="fasta"})}

  \item{format}{format argument to be passed to
  \code{ape::read.dna}}

  \item{interleaved}{interleaved argument to be passed to
  \code{ape::read.dna}}

  \item{gap}{character for gaps (to be passed to
  write.nexus.data)}

  \item{missing}{character for msising data (to be passed
  to write.nexus.data)}

  \item{partition.file}{if \code{NULL} nothing happens, if
  it's the path to a valid file is given this file is
  appended to the alignement. Particularly useful to
  include partition information in the NEXUS file.}

  \item{...}{}
}
\value{
nothing, used for its side of generating a NEXUS file
}
\description{
Converts an alignment from the PHYLIP or FASTA format to
the NEXUS format.
}
\details{
This function only takes alignments. It
}
\author{
Francois Michonneau
}

