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
#' @importFrom imager is.cimg threshold as.pixset shrink grow fill clean
#' @importFrom magrittr %>%
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
