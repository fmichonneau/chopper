

[![Build Status](https://travis-ci.org/fmichonneau/seqManagement.png?branch=master)](https://travis-ci.org/fmichonneau/seqManagement.png)

Latest Build log: https://travis-ci.org/fmichonneau/seqManagement

# seqManagement (temporary name)

This package (I'm still looking for a better name for it, as I misspell it
everytime) is a collection of tools to help manipulate and format alignment
files to be used for phylogenetic analyses. Overall, it is intended to faciliate
reproducible analyses by generating input files (alignment, partitions, ...) in
an automated way.

Feedback, comments, bug reports are all welcome and encouraged!

## Installation

This is an alpha, pre-release, package not available on CRAN. There is limited
testing, function names can change, and ~it's full of bugs~ there might be some
bugs. To install it, you need to get it from here (github).

Please note that this package may not work very well on Windows. It is developed
and tested on Linux, and probably works on Mac.

To make things easier, use the `install_github()` function from the `devtools`
package.

````
install.packages("devtools")
library(devtools)
install_github("fmichonneau/seqManagement")
````

More details soon.
