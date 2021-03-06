% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeAlignment.R
\name{mergeAlignment}
\alias{mergeAlignment}
\title{Merge alignment files.}
\usage{
mergeAlignment(listFiles, output, seqFolder)
}
\arguments{
\item{listFiles}{a list of fasta files to be put in the same
file. The path should be omitted and specified with the argument
\sQuote{seqFolder}.}

\item{output}{the path and name of the output file.}

\item{seqFolder}{the folder that holds the fasta files specified by \sQuote{listFiles}.}
}
\value{
TRUE if the function succeeded. This function is mostly
used for its side effect of merging different sequences/alignments.
}
\description{
Merge alignment files from identical markers.
}
\details{
Given a list of alignment files, this function puts them together
in a single file. The alignmenents must be from the same marker to
be relevant. Additionally, if the alignment want to be preserved,
they must be of the same length. Otherwise, you'll need to re-run
an alignment program on the output file of this function. This
function tests that the specified \sQuote{output} file doesn't
exist. If it does, the function stops with an error.
}
\author{
Francois Michonneau
}

