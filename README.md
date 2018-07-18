[![Travis-CI Build Status](https://travis-ci.org/MahShaaban/colocr.svg?branch=master)](https://travis-ci.org/MahShaaban/colocr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/colocr?branch=master&svg=true)](https://ci.appveyor.com/project/MahShaaban/colocr)
[![Coverage Status](https://img.shields.io/codecov/c/github/MahShaaban/colocr/master.svg)](https://codecov.io/github/MahShaaban/colocr?branch=master)

# colocr

An R package for conducting co-localization analysis.

## Overview

A few R packages are available for conducting image analysis, which is a very wide topic. As a result, some of use might feel at loss when all what they want to do is a simple co-localization calculations on a small number of microscopy images. This package provides a simple straight forward workflow for loading images, choosing regions of interest (ROIs) and calculating colocalization statistics. Included in the package, is a [shiny app](https://shiny.rstudio.com) that can be invoked locally to interactively select the regions of interest in a semi-automatic way. The package is based on the infamous [`imager`](https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html).


## Installing `colocr`

The package is under development and not yet on [CRAN](https://cran.r-project.org). To install the package from [github](https://github.com/MahShaaban/colocr) use the following.

```r
devtools::install_github('MahShaaban/colocr')
```


## Getting started

To get started, load the required packages and the images.
Then, apply the appropriate parameters for choosing the regions of interest
using the `parameter_choose`. Finally, check the appropriatness of the 
parameters by highlighting the ROIs on the image.

```r
# load libraries
library(imager)
library(colocr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- load.image(fl)
img.g <- grayscale(img)

# choose parameters
px <- parameter_choose(img.g, threshold = 90)

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
implements two simple colocalizations statistics; Pearson's Coefficeint Correlation [(PCC)](https://www.ncbi.nlm.nih.gov/pubmed/20653013) and the Manders Overlap Coefficient [(MOC)](https://www.ncbi.nlm.nih.gov/pubmed/20653013).

To apply both measures of correlation, we first load the images from the two channels and call `coloc_test`.

```r
fl <- system.file('extdata', 'Image0001_C002.jpg', package = 'colocr')
img1 <- load.image(fl)

fl <- system.file('extdata', 'Image0001_C003.jpg', package = 'colocr')
img2 <- load.image(fl)

corr <- coloc_test(img1, img2, px, type = 'all')

corr$p  # PCC
corr$r  # MPC
```

The same analysis and more can be conducted using a web interface for the package available [here](https://mahshaaban.shinyapps.io/colocr_app/)
## Acknowledgement

## More
