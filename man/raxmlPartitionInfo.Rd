\name{raxmlPartitionInfo}
\alias{raxmlPartitionInfo}
\title{Information from a RAxML partition file}
\usage{
raxmlPartitionInfo(partfile)
}
\arguments{
  \item{partfile}{Path of a RAxML partition file}
}
\value{
A list that includes the elements: \describe{
\item{\code{file}}{the partition file name}
\item{\code{locusName}}{a vector of character indicating
the name of each partition} \item{\code{locusStart}}{a
vector of numeric indicating the first position in the
alignment for the locus} \item{\code{locusEnd}}{a vector of
numeric indicating the last position in the alignement of
the locus}}
}
\description{
This function takes a RAxML partition file and returns
information about it as a list.
}
\details{
Currently, this function ignores the type of data of each
partition, and does not support the codon notation (e.g.,
1-300\3).
}
\author{
Francois Michonneau
}

