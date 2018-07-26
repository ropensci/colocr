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
#'
#' # choose parameters
#' px <- parameter_choose(img, threshold = 90)
#'
#' # highlight chosen region of interest
#' plot(img)
#' highlight(px)
#'
#' @importFrom imager is.cimg grayscale threshold as.pixset shrink grow fill clean
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

  # change image to gray scale
  img.g <- grayscale(img)

  # apply threshold
  img.t <- threshold(img.g, paste0(threshold, '%'))

  # change to pixset
  px <- as.pixset(1-img.t)

  # apply shrink
  px.m <- shrink(px, shrink)

  # apply grow
  px.m <- grow(px.m, grow)

  # apply fill
  px.m <- fill(px.m, fill)

  # apply clean
  px.m <- clean(px.m, clean)

  # return px.m
  return(px.m)
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
#'
#' # choose parameters
#' px <- parameter_choose(img, threshold = 90)
#'
#' # call coloc_test
#' parameter_show(img, px)
#'
#' @importFrom imager highlight channel
#' @importFrom graphics par plot text
#' @importFrom stats aggregate median
#'
#' @export
parameter_show <- function(img, px, labels, ind = c(1,2)) {
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
    if(!is.cimg(labels)) {
      stop('labels needs to be a a cimg object.')
    }

    # change to pixset
    px <- as.pixset(labels)

    # get positions for labels
    px.labs <- as.data.frame(labels)
    px.labs <- px.labs[px.labs$value != 0,]
    px.labs <- aggregate(px.labs[, c('x', 'y')],
                         by = list(value = px.labs$value),
                         FUN = median)

    # draw labels
    text(px.labs$x, px.labs$y, labels = px.labs$value, col = 'yellow')
  }

  # channel one highlighted
  img1 <- channel(img, ind = ind[1])
  plot(img1,
       axes = FALSE,
       main = 'Channel One')
  highlight(px)

  # channel two highlighted
  img2 <- channel(img, ind = ind[2])
  plot(img2,
       axes = FALSE,
       main = 'Channel Two')
  highlight(px)
}

#' Make labels
#'
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' @param n A \code{numeric} of the number of regions of interest
#'
#' @return An object of class \code{\link[imager]{cimg}}
#'
#' @importFrom imager label as.cimg
#' @importFrom stats reorder
#'
#' @export
labels_add <- function(px, tolerance = .1, n = 1) {
  px.labs <- label(px, tolerance = tolerance)
  value <- as.data.frame(px.labs)$value

  ids <- reorder(value, value, length)

  k <- levels(ids)

  k <- k[(length(k)-1):(length(k)-n)]

  new.ids <- ifelse(value %in% as.numeric(k), value, 0)
  f <- as.numeric(factor(new.ids))

  new.px <- as.data.frame(px.labs)
  new.px$value <- f - 1
  new.px <- as.cimg(new.px, dim = dim(px))

  return(new.px)
}
