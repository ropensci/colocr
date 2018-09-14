[![Travis-CI Build Status](https://travis-ci.org/MahShaaban/colocr.svg?branch=pipeable)](https://travis-ci.org/MahShaaban/colocr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/colocr?branch=pipeable&svg=true)](https://ci.appveyor.com/project/MahShaaban/colocr)
[![Coverage Status](https://img.shields.io/codecov/c/github/MahShaaban/colocr/pipeable.svg)](https://codecov.io/github/MahShaaban/colocr?branch=pipeable)
[![Build Status](https://travis-ci.org/MahShaaban/colocr_app.svg?branch=pipeable)](https://travis-ci.org/MahShaaban/colocr_app)
[![status](https://img.shields.io/badge/shinyapps.io-running-green.svg)](https://mahshaaban.shinyapps.io/colocr_app/) 
[![](https://badges.ropensci.org/243_status.svg)](https://github.com/ropensci/onboarding/issues/243)

# colocr

An R package for conducting co-localization analysis.

## Overview

A few R packages are available for conducting image analysis, which is a very wide topic. As a result, some of us might feel at a loss when all they want to do is a simple co-localization calculations on a small number of microscopy images. This package provides a simple straight forward workflow for loading images, choosing regions of interest (ROIs) and calculating co-localization statistics. Included in the package, is a [shiny app](https://shiny.rstudio.com) that can be invoked locally to interactively select the regions of interest in a semi-automatic way. The package is based on the R package [`imager`](https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html).


## Installing `colocr`

The package development version is available at [github](https://github.com/MahShaaban/colocr).

```
# install from github (pipeable branch)
devtools::install_github('MahShaaban/colocr@pipeable')
```


## Getting started

To get started, load the required packages and the images. The images below
are from [DU145](https://en.wikipedia.org/wiki/DU145) cell line and were 
stained for two proteins; [RKIP](https://en.wikipedia.org/wiki/Raf_kinase_inhibitor_protein) and [LC3](https://en.wikipedia.org/wiki/MAP1LC3B).
Then, apply the appropriate parameters for choosing the regions of interest
using the `roi_select`. Finally, check the appropriateness of the 
parameters by highlighting the ROIs on the image.

```
# load libraries
library(colocr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- image_load(fl)

# select ROI and show the results
par(mfrow = c(2,2), mar = rep(1, 4))

img %>%
  roi_select(threshold = 90) %>%
  roi_show()
```

The same can be achieved interactively using an accompanying **shiny** app.
To launch the app run.

```{r run_app, eval=FALSE}
run_app()
```

The reset of the analysis depends on the particular kind of images. Now, `colocr`
implements two simple co-localization statistics; Pearson's Coefficient Correlation [(PCC)](https://www.ncbi.nlm.nih.gov/pubmed/20653013) and the Manders Overlap Coefficient [(MOC)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3074624/).

To apply both measures of correlation, we first get the pixel intensities and call `roi_test` on the merge image.

```
# calculate co-localization statistics
img %>%
  roi_select(threshold = 90) %>%
  roi_test(type = 'both')
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
