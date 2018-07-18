[![Travis-CI Build Status](https://travis-ci.org/MahShaaban/colocr.svg?branch=master)](https://travis-ci.org/MahShaaban/colocr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/MahShaaban/colocr?branch=master&svg=true)](https://ci.appveyor.com/project/MahShaaban/colocr)
[![Coverage Status](https://img.shields.io/codecov/c/github/MahShaaban/colocr/master.svg)](https://codecov.io/github/MahShaaban/colocr?branch=master)

# colocr

An R package for conducting co-localization analysis.

Install package from github

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
plot(img)
highlight(px)
```

The same can be acheived interactively using an accompanying **shiny** app.
To launch the app run.

```r
run_app()
```

The reset of the anlysis depends on the particular kind of images. Now, `colocr`
implements two simple colocalizations statistics; Pearson's Coefficeint Correlation (PCC) and the Manders Overlap Coefficient (POC).

Try the shiny app [here](https://mahshaaban.shinyapps.io/colocr_app/)
