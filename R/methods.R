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
#' @return A \code{\link[imager]{cimg}}. The original input \code{img} with an
#' additional attribute \code{label}. \code{label} is a \code{vector} of
#' \code{integer}s. The labels for the selected regions of interests starts
#' from 1 and 0 is ignored.
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
#' # load required libraries
#' library(imager)
#' library(magrittr)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#'
#' # choose ROI
#' newimg <- roi_select(img, threshold = 90)
#'
#' # check the ROI labels
#' unique(attr(newimg, 'label'))
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
    newimg <- img
    attr(newimg, 'label') <- as.numeric(labs.px)
  } else {
    newimg <- img
    attr(newimg, 'label') <- as.numeric(px.m)
  }
  return(newimg)
}

#' Show the selected regions of interest
#'
#' Show/highlight the selected regions of interest on different image channels
#'
#' @param img A \code{\link[imager]{cimg}} obeject such as the one returned
#' from \code{\link{roi_select}}
#' @param ind A \code{numeric} object of length two. For the channel indexes.
#'
#' @return NULL
#'
#' @details calling this function with \code{img} object which is returned from
#' \code{\link{roi_select}} returns four different plots. The original image, a
#' low resolution representation of the selected regions of interest and the
#' two channels indicated through \code{ind} highlighted.
#'
#' @examples
#' # load required libraries
#' library(imager)
#' library(magrittr)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#'
#' # choose and show ROI
#' par(mfrow=c(2,2))
#' roi_select(img, threshold = 90) %>%
#'   roi_show()
#'
#' @importFrom imager highlight channel cimg
#' @importFrom graphics plot
#'
#' @export
roi_show <- function(img, ind = c(1,2)) {
UseMethod('roi_show')
}

#' @export
roi_show.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
roi_show.cimg <- function(img, ind = c(1,2)) {

  # get labels from img
  # transform labels to cimg object
  labels <- attr(img, 'label')
  dims <- dim(grayscale(img))
  a <- array(labels, dim = dims)

  px <- cimg(a)

  # merge image
  plot(img,
       axes = FALSE,
       main = 'Merge')

  # pixset image
  plot(px,
       axes = FALSE,
       main = 'Pixel Set')

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

#' Show pixel intensities
#'
#' Show the pixel intensities of certain image channels
#'
#' @inheritParams roi_show
#'
#' @return NULL
#'
#' @details Calling this function returns two plots. The first is a scatter
#' plot of the pixel intensities from two channels. The second is the density
#' distribution of the intensities from the two channels.
#'
#' @examples
#' # load required libraries
#' library(imager)
#' library(magrittr)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#'
#' # choose ROI and show the pixel intensities
#' roi_select(img, threshold = 90) %>%
#'   intensity_show()
#'
#' @importFrom stats density
#' @importFrom scales alpha
#' @importFrom graphics plot lines
#'
#' @export
intensity_show <- function(img, ind = c(1,2)) {
  UseMethod('intensity_show')
}

#' @export
intensity_show.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
intensity_show.cimg <- function(img, ind = c(1,2)) {
  # get pixel intensities
  pix_int <- .intensity_get(img, ind = ind)

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
#' @inheritParams roi_show
#' @param type A \code{character} vector of the desired co-localization
#' statistics. Default is 'pearsons', other inputs are 'manders' or 'all'.
#'
#' @return A \code{list}.
#'
#' @details The co-localization stats requested in \code{type} is returned as
#' list items for each. When different labels are provided, the stats are
#' calculated for each label individually.
#'
#' @examples
#' # load required libraries
#' library(imager)
#' library(magrittr)
#'
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- load.image(fl)
#'
#' # choose roi and test colocalization
#' roi_select(img, threshold = 90) %>%
#'   coloc_test()
#'
#' @importFrom imager is.cimg is.pixset channel
#'
#' @export
coloc_test <- function(img, ind = c(1,2), type = 'pearsons') {
  UseMethod('coloc_test')
}

#' @export
coloc_test.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
coloc_test.cimg <- function(img, ind = c(1,2), type = 'pearsons') {
  # get pixel intensity
  pix_int <- .intensity_get(img, ind = ind)

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
