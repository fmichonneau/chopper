

[![Build Status](https://travis-ci.org/fmichonneau/chopper.png?branch=master)](https://travis-ci.org/fmichonneau/chopper.png)

Latest Build log: https://travis-ci.org/fmichonneau/chopper

# chopper

This package (formerly known as seqManagement) is a collection of tools to help
manipulate and format alignment files to be used for phylogenetic and population
genetics analyses. Overall, it is intended to faciliate reproducible analyses by
generating input files (alignment, partitions, ...) in ways that can be
scripted.

Feedback, comments, bug reports are welcome and encouraged!

## Installation

This is an alpha, pre-release, package not available on CRAN. Function names can
change, and there might be some bugs. To install it, you need to get it from
here (GitHub). This is under heavy development, so use at your own risks.

Please note that this package may not work very well on Windows. It is developed
and tested on Linux, and probably works on Mac.

To make things easier, use the `install_github()` function from the `devtools`
package.

````
install.packages("devtools")
library(devtools)
install_github("fmichonneau/chopper")
````

More details soon. In the meantime, feel free to open an issue if you have any questions.
