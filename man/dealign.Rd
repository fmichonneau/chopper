% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dealign.R
\name{dealign}
\alias{dealign}
\title{Dealign an alignment}
\usage{
dealign(file, output)
}
\arguments{
\item{file}{input file (in FASTA)}

\item{output}{output file (in FASTA)}
}
\value{
TRUE if function worked, nothing otherwise. Used for its
side effect of creating a file.
}
\description{
De-align an alignment
}
\details{
Removed all gaps in an alignment
}
\author{
Francois Michonneau
}

