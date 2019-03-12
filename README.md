# blackboxstudyR <img src='man/figures/logo.png' align='right' height='139'>

`blackboxstudyR` is an R package for fitting basic item response theory (IRT) models to FBI Black Box [Data](inst/extdata/TestResponses.txt).

## Installation

This package is not yet available on CRAN. Install the development version from Github with: 

```r
# install.packages("devtools")
devtools::install_github("aluby/blackboxstudyR")
```

## Usage 

Usage consists of three steps: (1) Score the data, (2) Format a data list, (3) Fit the IRT model. 

```r
im_scored = score_bb_data(TestResponses, "inconclusive_mcar")
im_data = irt_data_bb(TestResponses, im_scored)
im_model = fit_rasch(im_data)
```
