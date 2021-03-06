% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkAmbiguity.R
\name{checkAmbiguity}
\alias{checkAmbiguity}
\title{Check for ambiguities in a DNA sequence}
\usage{
checkAmbiguity(file, format = "fasta", quiet = FALSE, Namb = TRUE,
  simplify = TRUE, ...)
}
\arguments{
\item{file}{path and file name for a DNA alignment (character)}

\item{format}{format of the DNA alignment (passed to
\code{\link[ape]{read.dna}})}

\item{quiet}{if \code{FALSE}, outputs as messages the ambiguities
found (logical)}

\item{Namb}{should Ns be considered ambiguities? (logical)}

\item{simplify}{if \code{TRUE}, the returned object is reduced to
only include the sequences that contain ambiguities. If
\code{FALSE}, the returned object has the same length as the
number of sequences in the alignment.}

\item{...}{additional arguments to be passed to ape:::read.dna}
}
\value{
If there are no ambiguities, the function returns a
zero-length integer vector. Otherwise, it returns a named list
which includes for each sequence, the position in the alignment of
the ambiguities for each sequence that has one or more ambiguities
(if \code{simplify=TRUE}), and is named using the ambiguity. if
quiet=FALSE, this vector is returned invisibly in addition of the
}
\description{
Check for ambiguities or other characters that are not allowed in
haplotypes.
}
\details{
This function takes a DNA alignment and looks for characters in
the sequence that are not A, C, T, G, -, ? (and optionally N). It
ignores case.
}
\author{
Francois Michonneau
}

