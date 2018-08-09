[![Travis-CI Build Status](https://travis-ci.org/MahShaaban/colocr.svg?branch=master)](https://travis-ci.org/MahShaaban/colocr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/colocr?branch=master&svg=true)](https://ci.appveyor.com/project/MahShaaban/colocr)
[![Coverage Status](https://img.shields.io/codecov/c/github/MahShaaban/colocr/master.svg)](https://codecov.io/github/MahShaaban/colocr?branch=master)
[![Build Status](https://travis-ci.org/MahShaaban/colocr_app.svg?branch=master)](https://travis-ci.org/MahShaaban/colocr_app)
[![status](https://img.shields.io/badge/shinyapps.io-running-green.svg)](https://mahshaaban.shinyapps.io/colocr_app/) 
[![](https://badges.ropensci.org/243_status.svg)](https://github.com/ropensci/onboarding/issues/243)

# colocr

An R package for conducting co-localization analysis.

## Overview

A few R packages are available for conducting image analysis, which is a very wide topic. As a result, some of us might feel at loss when all they want to do is a simple co-localization calculations on a small number of microscopy images. This package provides a simple straight forward workflow for loading images, choosing regions of interest (ROIs) and calculating co-localization statistics. Included in the package, is a [shiny app](https://shiny.rstudio.com) that can be invoked locally to interactively select the regions of interest in a semi-automatic way. The package is based on the R package [`imager`](https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html).


## Installing `colocr`

The package development version is available at [github](https://github.com/MahShaaban/colocr).

```r
# install from development branch (no submodules!)
devtools::install_github('MahShaaban/colocr@development')
```

## Getting started

To get started, load the required packages and the images.
Then, apply the appropriate parameters for choosing the regions of interest
using the `roi_select`. Finally, check the appropriatness of the 
parameters by highlighting the ROIs on the image.

```r
# load libraries
library(imager)
library(colocr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- load.image(fl)

# choose parameters
px <- roi_select(img, threshold = 90)

# highlight chosen region of interest
par(mar=rep(0, 4))
plot(img, axes = FALSE)
highlight(px)
```

The same can be acheived interactively using an accompanying **shiny** app.
To launch the app run.

```r
run_app()
```

The reset of the anlysis depends on the particular kind of images. Now, `colocr`
implements two simple colocalizations statistics; Pearson's Coefficeint Correlation [(PCC)](https://www.ncbi.nlm.nih.gov/pubmed/20653013) and the Manders Overlap Coefficient [(SCC)](https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient).

To apply both measures of correlation, we first get the pixel intensities and call `coloc_test` on the merge image.

```r
pix_int <- intensity_get(img, px)
corr <- coloc_test(pix_int, type = 'all')

corr$p  # PCC
corr$r  # MOC
```

The same analysis and more can be conducted using a web interface for the package available [here](https://mahshaaban.shinyapps.io/colocr_app2/)

## Acknowledgement

* The vignette images from [Lai Huyen Trang](https://www.researchgate.net/profile/Lai_Huyen_Trang)  
* The test examples from [Colocalization Benchmark Source (CBS)](https://www.colocalization-benchmark.com/index.html)  
* The implementation of the co-localization statistics from [Dunn et al.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3074624/)  

## More

```r
browseVignettes('colocr')
```
