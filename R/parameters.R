#' Choose Parameter for selecting ROIs
#'
#' Choose the different parameters to select the regions of interest (ROI)
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param threshold A \code{numeric} to be passed to
#' \code{\link[imager]{threshold}}
#' @param shrink A \code{numeric} to be passed to \code{\link[imager]{shrink}}
#' @param grow A \code{numeric} to be passed to \code{\link[imager]{grow}}
#' @param fill A \code{numeric} to be passed to \code{\link[imager]{fill}}
#' @param clean A \code{numeric} to be passed to \code{\link[imager]{clean}}
#'
#' @return A \code{\link[imager]{pixset}}
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

#' Show the selected ROIs
#'
#' Show/highlight the selected ROIs for a set of parameters
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
#' @importFrom graphics par plot text
#' @importFrom stats median
#'
#' @export
parameter_show <- function(img, img1, img2, px, labels) {
  par(mfrow = c(2,2), mar=rep(1, 4))

  # merge image
  plot(img,
       axes = FALSE,
       main = 'Merge')

  # pixset image
  plot(px,
       axes = FALSE,
       main = 'Pixel Set')

  # add labels when TRUE
  if(!missing(labels)) {
    if(!is.data.frame(labels)) {
      stop('labels needs to be a data.frame of 3 columns; x, y and value.')
    }

    px <- as.cimg(labels)

    px.labs <- labels %>%
      group_by(value) %>%
      summarise_all(median)

    text(px.labs$x, px.labs$y, labels = px.labs$value, col = 'yellow')
  }

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

#' Make labels data.frame
#'
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' @param n A \code{numeric} of the number of regions of interest
#'
#' @return A \code{data.frame} of three columns x, y and value.
#'
#' @importFrom imager label
#' @importFrom dplyr filter group_by summarise arrange desc mutate full_join select
#' @importFrom magrittr %>%
#'
#' @export
labels_add <- function(px, tolerance = .1, n = 1) {
  px.labs <- label(px, tolerance = tolerance) %>%
    as.data.frame()

  df <- px.labs %>%
    filter(value != 0) %>%
    group_by(value) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(id = 1:n()) %>%
    full_join(px.labs) %>%
    mutate(value = id) %>%
    select(value, x, y) %>%
    filter(value <= n)

  return(df)
}
