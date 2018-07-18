#' Choose Parameter for selecting ROI
#'
#' Choose the different parameter to select the regions of interest (ROI)
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param threshold A numeric to be passed to threshold
#' @param shrink A numeric to be passed to shrink
#' @param grow A numeric to be passed to grow
#' @param fill A numeric to be passed to fill
#' @param clean A numeric to be passed to clean
#'
#' @return A pixset
#'
#' @examples
#' # load libraries
#' library(imager)
#' library(colocr)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#' img.g <- grayscale(img)
#'
#' # choose parameters
#' px <- parameter_choose(img.g, threshold = 90)
#'
#' # highlight chosen region of interest
#' plot(img)
#' highlight(px)
#'
#' @importFrom imager is.cimg threshold as.pixset shrink grow fill clean
#' @importFrom magrittr %>%
#'
#' @export
parameter_choose <- function(img, threshold, shrink = 5, grow = 5, fill = 5,
                             clean = 5) {
  if(!is.cimg(img)) {
    stop('img should be of class cimg.')
  }
  if(!is.numeric(threshold)) {
    stop('threshold should be a numeric between 0 and 100.')
  }

  img.t <- threshold(img, paste0(threshold, '%'))

  px <- as.pixset(1-img.t)

  px.m <- px %>%
    shrink(shrink) %>%
    grow(grow) %>%
    fill(fill) %>%
    clean(clean)
  px.m
}

#' Show the selected ROI for set of parameters
#'
#' @inheritParams parameter_choose
#' @inheritParams coloc_test
#'
#' @return NULL
#'
#' @examples
#' # load libraries
#' library(imager)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#' img.g <- grayscale(img)
#'
#' # choose parameters
#' px <- parameter_choose(img.g, threshold = 90)
#'
#' # load images from two channels
#' img1 <- load.image(system.file('extdata/', 'Image0001_C002.jpg', package = 'colocr'))
#' img2 <- load.image(system.file('extdata/', 'Image0001_C003.jpg', package = 'colocr'))
#'
#' # call coloc_test
#' parameter_show(img, img1, img2, px)
#'
#' @importFrom imager highlight
#' @importFrom graphics par plot
#'
#' @export
parameter_show <- function(img, img1, img2, px) {
  par(mfrow = c(2,2), mar=rep(1, 4))

  # merge image
  plot(img,
       axes = FALSE,
       main = 'Merge')

  # pixset image
  plot(px,
       axes = FALSE,
       main = 'Pixel Set')

  # channel one highlighted
  plot(img1,
       axes = FALSE,
       main = 'Channel One')
  highlight(px)

  # channel two highlighted
  plot(img2,
       axes = FALSE,
       main = 'Channel Two')
  highlight(px)
}
