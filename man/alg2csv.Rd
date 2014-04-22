\name{alg2csv}
\alias{alg2csv}
\title{algToCsv (alignemnt to CSV)}
\usage{
alg2csv(file, output, format = "fasta")
}
\arguments{
  \item{file}{path and file name for a DNA alignement.}

  \item{output}{path and file name of the CSV file which
  will contain the alignement.}

  \item{format}{format of the alignement specified by the
  \sQuote{file} argument.}
}
\value{
TRUE if the function succeeded, and writes the
\sQuote{output} file where specified.
}
\description{
Converts an alignment into a CSV file.
}
\details{
This function converts an alignement (in FASTA format by
default, but accepts all formats accepted by
ape:::read.dna) into a 2-column CSV file, where the first
column is the sequence name, and the second column the
actual sequence. The sequences are ordered alphabetically,
and converted to upper case.
}
\author{
Francois Michonneau
}

