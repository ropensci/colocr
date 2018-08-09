#' Select regions of interest
#'
#' Select regions of interest in an image using different morphological operations
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param threshold A \code{numeric} to be passed to
#' \code{\link[imager]{threshold}}
#' @param shrink A \code{numeric} to be passed to \code{\link[imager]{shrink}}
#' @param grow A \code{numeric} to be passed to \code{\link[imager]{grow}}
#' @param fill A \code{numeric} to be passed to \code{\link[imager]{fill}}
#' @param clean A \code{numeric} to be passed to \code{\link[imager]{clean}}
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' @param n A \code{numeric} of the number of regions of interest
#'
#' @return A \code{\link[imager]{pixset}} or a labeled image \code{\link[imager]{cimg}}
#'
#' @details The function applies several \code{\link{imager}} morphological
#' manipulations to select the regions of interest. These include
#' \code{\link[imager]{threshold}} which sets all values below certain cut to
#' 0; \code{\link[imager]{shrink}}/\code{\link[imager]{grow}} for pixel set
#' dilation and erosion; \code{\link[imager]{fill}}\\code{\link[imager]{clean}}
#' for removing isolated regions and holes. When \code{n} is provided, the
#' individual regions (connected components) are selected where \code{tolerance}
#' is used to determine if two pixels belong to the same region.
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
#' px <- roi_select(img, threshold = 90)
#'
#' # highlight chosen region of interest
#' plot(img)
#' highlight(px)
#'
#' @importFrom imager is.cimg grayscale threshold as.pixset shrink grow fill clean
#'
#' @export
roi_select <- function(img, threshold, shrink = 5, grow = 5, fill = 5,
                             clean = 5, tolerance = .1, n) {
  UseMethod('roi_select')
}

#' @export
roi_select.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
roi_select.cimg <- function(img, threshold, shrink = 5, grow = 5, fill = 5,
                                  clean = 5, tolerance = .1, n) {

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

  # add labels when n is provided
  if(!missing(n)) {
    labs.px <- .labels_add(px.m, tolerance = tolerance, n = n)
    return(labs.px)
  } else {
    return(px.m)
  }
}

#' Show the selected regions of interest
#'
#' Show/highlight the selected regions of interest on different image channels
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param labels An object of class \code{\link[imager]{cimg}}
#' @param ind A \code{numeric} object of length two. For the channel indexes.
#'
#' @return NULL
#'
#' @details Calling this function on an image and a \code{px}, \code{\link[imager]{pixset}}
#' of the same dimensions returns four different plots. The original image, a
#' a low resolution representation of the \code{\link[imager]{pixset}} and the
#' two channels indicated through \code{ind} highlighted. Additionally, when
#' labels are provided through \code{labels} the regions are individually
#' labeled.
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
#' px <- roi_select(img, threshold = 90)
#'
#' # call coloc_test
#' roi_show(img, px)
#'
#' @importFrom imager highlight channel
#' @importFrom graphics plot text
#' @importFrom stats aggregate median
#'
#' @export
roi_show <- function(img, px, labels, ind = c(1,2)) {
UseMethod('roi_show')
}

#' @export
roi_show.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
roi_show.cimg <- function(img, px, labels, ind = c(1,2)) {
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

#' Get pixel intensities
#'
#' Get the pixel intensities of certain image channels
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}} or with labels
#' \code{\link[imager]{cimg}}
#' @param ind A \code{numeric} of length two for channel indexes
#'
#' @return A \code{list} of three items. The first two items are the values of
#' the pixel intensities of the channels indicated by \code{ind}. The third is
#' the labels of the individual regions of interest when a labeled \code{px}
#' is provided. Otherwise, the value of labels is set to 1.
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
#' px <- roi_select(img, threshold = 90)
#'
#' # call coloc_test
#' pix_int <- intensity_get(img, px)
#'
#' @importFrom imager channel is.cimg as.pixset is.pixset
#'
#' @export
intensity_get <- function(img, px, ind = c(1,2)) {
  UseMethod('intensity_get')
}

#' @export
intensity_get.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
intensity_get.cimg <- function(img, px, ind = c(1,2)) {

  # get channel intensities
  img1.g <- channel(img, ind = ind[1])
  img2.g <- channel(img, ind = ind[2])

  # use labels to subset images when provided
  if(is.cimg(px)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[as.pixset(px)])
    img2.num <- as.numeric(img2.g[as.pixset(px)])

    # calculate correlations
    f <- as.data.frame(px)
    f <- f[f$value != 0,]
    f <- f$value

    # make list
    pixel_int <- list(channel1 = img1.num,
                      channel2 = img2.num,
                      labels = f)
  } else if(is.pixset(px)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[px])
    img2.num <- as.numeric(img2.g[px])

    # make list
    pixel_int <- list(channel1 = img1.num,
                      channel2 = img2.num,
                      labels = rep(1, length(img1.num)))
  }

  # retrun corr
  return(pixel_int)
}

#' Show pixel intensities
#'
#' Show the pixel intensities of certain image channels
#'
#' @param pix_int A list, such as that returned by \code{\link{intensity_get}}
#'
#' @return NULL
#'
#' @details Calling this function returns two plots. The first is a scatter
#' plot of the pixel intensities from two channels. The second is the density
#' distribution of the intensities from the two channels.
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
#' px <- roi_select(img, threshold = 90)
#'
#' # call coloc_test
#' pix_int <- intensity_get(img, px)
#'
#' # call intensity_show
#' intensity_show(pix_int)
#'
#' @importFrom stats density
#' @importFrom scales alpha
#' @importFrom graphics plot lines
#'
#' @export
intensity_show <- function(pix_int) {
  if(!is.list(pix_int)) {
    stop('pix_int should be a list, output of coloc_test.')
  }

  # scatter plot
  plot(pix_int[[1]], pix_int[[2]],
       col = alpha(pix_int[[3]], 0.5),
       pch = 16,
       xlab = 'Channel One', ylab = 'Channel Two')

  # density plot
  d1 <- density(pix_int[[1]])
  d2 <- density(pix_int[[2]])
  xlim <- c(min(c(d1$x, d2$x)), max(c(d1$x, d2$x)))
  ylim <- c(min(c(d1$y, d2$y)), max(c(d1$y, d2$y)))
  plot(d1$x, d1$y,
       xlim = xlim, ylim = ylim,
       type = 'l', col = alpha('green', .5),
       xlab = 'Pixel Value', ylab = 'Density')
  lines(d2$x, d2$y,
        col = alpha('red', .5))
}

#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @inheritParams intensity_show
#' @param type A \code{character}; "pearsons", "manders" or "all"
#'
#' @return A \code{list}.
#'
#' @details The co-localization stats requested in \code{type} is returned as
#' list items for each. When different labels are provided, the stats are
#' calculated for each label individually.
#'
#' @examples
#' # load libraries
#' library(imager)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#'
#' # choose roi
#' px <- roi_select(img, threshold = 90)
#'
#' # get intensities
#' pix_int <- intensity_get(img, px)
#'
#' # call coloc_test
#' coloc_test(pix_int)
#'
#' @importFrom imager is.cimg is.pixset channel
#'
#' @export
coloc_test <- function(pix_int, type = 'pearsons') {

  if(!type %in% c('pearsons', 'manders', 'all')) {
    stop('type takes one of these values; pearsons, manders or all')
  }

  # unpack pix_int
  img1.num <- pix_int[[1]]
  img2.num <- pix_int[[2]]
  f <- pix_int[[3]]

  # use labels to subset images when provided
  if(length(unique(pix_int[[3]])) > 1) {

    # split pixels by labels
    ll <- lapply(list(img1.num, img2.num), split, f = f)

    corr <- switch (type,
                    'pearsons' = list(p = unlist(mapply(function(x, y) {
                      .pearson(x, y)
                      }, ll[[1]], ll[[2]]))),
                    'manders' = list(r = unlist(mapply(function(x, y) {
                      .manders(x, y)
                      }, ll[[1]], ll[[2]]))),
                    'all' = list(p = unlist(mapply(function(x, y) {
                      .pearson(x, y)
                      }, ll[[1]], ll[[2]])),
                                 r = unlist(mapply(function(x, y) {
                                   .manders(x, y)
                                   }, ll[[1]], ll[[2]])))
    )
  } else {
    # calculate correlations
    corr <- switch (type,
                    'pearsons' = list(p = .pearson(img1.num, img2.num)),
                    'manders' = list(r = .manders(img1.num, img2.num)),
                    'all' = list(p = .pearson(img1.num, img2.num),
                                 r = .manders(img1.num, img2.num))
    )
  }

  # retrun corr
  return(corr)
}
