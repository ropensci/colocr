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
# install from pipeable branch
devtools::install_github('MahShaaban/colocr@pipeable')
```
