This archive contains an interface to AnVIL web services. The AnVIL
package has been available since Bioconductor version 3.11. To install
the released version, follow instructions in the package vignette

    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    BiocManager::install("AnVIL")

To install the development (github master) version in a recent _R_,
use

    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    if (!requireNamespace("remotes", quietly = TRUE))
        install.packages("remotes")
    BiocManager::install("Bioconductor/AnVIL")

View the vignette (on [Bioconductor][bioc-vignette],
[github][github-vignette], or in your R session
`browseVignettes(package = "AnVIL")`) for usage and help pages for
accurate documentation.  Visit the Bioconductor package [landing page]
for more information.

[landing page]: https://bioconductor.org/packages/AnVIL
[bioc-vignette]: https://bioconductor.org/packages/devel/bioc/vignettes/AnVIL/inst/doc/Introduction.html
[github-vignette]: https://github.com/Bioconductor/AnVIL/blob/master/vignettes/Introduction.Rmd
