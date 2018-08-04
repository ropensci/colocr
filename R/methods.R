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
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' @param n A \code{numeric} of the number of regions of interest
#'
#' @return A \code{\link[imager]{pixset}} or a labeled image \code{\link[imager]{cimg}}
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

  # add labels when n is povided
  if(!missing(n)) {
    labs.px <- .labels_add(px.m, tolerance = tolerance, n = n)
    return(labs.px)
  } else {
    return(px.m)
  }
}

#' Show the selected ROIs
#'
#' Show/highlight the selected ROIs for a set of parameters
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param labels An object of class \code{\link[imager]{cimg}}
#' @param ind A \code{numeric} object of length two. For the channel indicies.
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

#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}} or with labels
#' \code{\link[imager]{cimg}}
#' @param type A \code{character}; "pearsons", "manders" or "all"
#' @param num A \code{logical}; return the \code{numeric} values of the images
#' or not
#' @param ind A \code{numeric} of length two for channel indecies
#'
#' @return A \code{list} of one or two correlations measures and two
#' \code{numeric}s when num is TRUE
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
#' coloc_test(img, px)
#'
#' @importFrom imager is.cimg is.pixset channel
#'
#' @export
coloc_test <- function(img, px, type = 'pearsons', num = FALSE, ind = c(1,2)) {
  UseMethod('coloc_test')
}

#' @export
coloc_test.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
coloc_test.cimg <- function(img, px, type = 'pearsons', num = FALSE, ind = c(1,2)) {
  if(!is.cimg(img)) {
    stop('img should be of class cimg.')
  }

  # change images to gray scal
  img1.g <- channel(img, ind = ind[1])
  img2.g <- channel(img, ind = ind[2])

  if(!type %in% c('pearsons', 'manders', 'all')) {
    stop('type takes one of these values; pearsons, manders or all')
  }

  # use labels to subset images when provided
  if(is.cimg(px)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[as.pixset(px)])
    img2.num <- as.numeric(img2.g[as.pixset(px)])

    # calculate correlations
    f <- as.data.frame(px)
    f <- f[f$value != 0,]
    f <- f$value

    # split pixels by labels
    ll <- lapply(list(img1.num, img2.num), split, f = f)

    corr <- switch (type,
                    'pearsons' = list(p = unlist(mapply(function(x, y) .pearson(x, y), ll[[1]], ll[[2]]))),
                    'manders' = list(r = unlist(mapply(function(x, y) .manders(x, y), ll[[1]], ll[[2]]))),
                    'all' = list(p = unlist(mapply(function(x, y) .pearson(x, y), ll[[1]], ll[[2]])),
                                 r = unlist(mapply(function(x, y) .manders(x, y), ll[[1]], ll[[2]])))
    )
  } else if(is.pixset(px)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[px])
    img2.num <- as.numeric(img2.g[px])

    # calculate correlations
    corr <- switch (type,
                    'pearsons' = list(p = .pearson(img1.num, img2.num)),
                    'manders' = list(r = .manders(img1.num, img2.num)),
                    'all' = list(p = .pearson(img1.num, img2.num),
                                 r = .manders(img1.num, img2.num))
    )
  }

  # add the numeric values when num == TRUE
  if (num) {
    corr$channel1 = img1.num
    corr$channel2 = img2.num
  }
  if(is.cimg(px)) {
    corr$labels = f
  } else if(!is.cimg(px) && num){
    corr$labels <- rep(1, length(corr$channel1))
  }

  # retrun corr
  return(corr)
}

#' Show the colocalization output
#'
#' @param corr A list, such as that returned by coloc_test with num == TRUE
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
#' px <- roi_select(img, threshold = 90)
#'
#' # call coloc_test
#' corr <- coloc_test(img, px, num = TRUE)
#'
#' # call coloc_show
#' coloc_show(corr)
#'
#' @importFrom stats density
#' @importFrom scales alpha
#' @importFrom graphics plot lines
#'
#' @export
coloc_show <- function(corr) {
  if(!is.list(corr)) {
    stop('corr should be a list, output of coloc_test.')
  }
  if(length(corr) < 3) {
    stop('make sure to call coloc_test with num == TRUE.')
  }

  # scatter plot
  plot(corr$channel1, corr$channel2,
       col = alpha(corr$labels, 0.5),
       pch = 16,
       xlab = 'Channel One', ylab = 'Channel Two')

  # density plot
  d1 <- density(corr$channel1)
  d2 <- density(corr$channel2)
  xlim <- c(min(c(d1$x, d2$x)), max(c(d1$x, d2$x)))
  ylim <- c(min(c(d1$y, d2$y)), max(c(d1$y, d2$y)))
  plot(d1$x, d1$y,
       xlim = xlim, ylim = ylim,
       type = 'l', col = alpha('green', .5),
       xlab = 'Pixel Value', ylab = 'Density')
  lines(d2$x, d2$y,
        col = alpha('red', .5))
}
