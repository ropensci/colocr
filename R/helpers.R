#' Get pixel intensities
#'
#' Get the pixel intensities of certain image channels
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param ind A \code{numeric} of length two for channel indexes
#'
#' @return A \code{list} of three items. The first two items are the values of
#' the pixel intensities of the channels indicated by \code{ind}. The third is
#' the labels of the individual regions of interest.
#'
#' @examples
#' # load image
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
#'
#' # choose parameters
#' int <- roi_select(img, threshold = 90) %>%
#'   .intensity_get()
#'
#' @importFrom imager channel is.cimg as.pixset is.pixset cimg
#'
#' @export
.intensity_get <- function(img, ind = c(1,2)) {
  UseMethod('.intensity_get')
}

#' @export
.intensity_get.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg object."))
}

#' @export
.intensity_get.cimg <- function(img, ind = c(1,2)) {

  # get labels from img
  # transform labels to cimg object
  labels <- attr(img, 'label')
  dims <- dim(grayscale(img))
  a <- array(labels, dim = dims)

  px <- cimg(a)

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

#' Label regions of interest
#'
#' Add labels to regions of interest in an image
#'
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' @param n A \code{numeric}, the number of desired regions of interest
#'
#' @return An object of class \code{\link[imager]{cimg}}. The labels are coded
#' the values in the object starting from 1. The rest of the image is labeled 0.
#'
#' @importFrom imager label as.cimg
#' @importFrom stats reorder
#'
#' @export
.labels_add <- function(px, tolerance, n) {
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

#' Calculate Pearson's Correlation Coefficient
#'
#' Calculates the Pearson's correlation coefficient between two numeric vectors
#'
#' @param r A \code{numeric} vector
#' @param g A \code{numeric} vector
#'
#' @return A \code{numeric} of length one.
#'
#' @examples
#' set.seed(123)
#' r <- rnorm(10)
#'
#' set.seed(1234)
#' g <- rnorm(10)
#'
#' .pearson(r, g)
#'
#' @export
.pearson <- function(r, g) {
  rx <- r - mean(r)
  gx <- g - mean(g)

  nom <- sum(rx * gx)
  den <- sqrt(sum(rx**2) * sum(gx**2))
  nom/den
}

#' Calculate Marnders Overlap Coefficient
#'
#' Calculates the manders overlap coefficient between two numeric vectors
#'
#' @param r A \code{numeric} vector
#' @param g A \code{numeric} vector
#'
#' @return A \code{numeric} of length one.
#'
#' @examples
#' set.seed(123)
#' r <- rnorm(10)
#'
#' set.seed(1234)
#' g <- rnorm(10)
#'
#' .manders(r, g)
#'
#' @export
.manders <- function(r, g) {
  nom <- sum(r * g)
  den <- sqrt(sum(r**2) * sum(g**2))
  nom/den
}

#' Load images from files
#'
#' A wrap around \code{\link[magick]{image_read}} and
#' \code{\link[imager]{magick2cimg}} to load one or more images from files.
#'
#' @param image_file A \code{character} vector of one or more paths to image
#' files
#'
#' @return A \code{cimg} object or a \code{list} of \code{cimg} objects when
#' multiple files are passed to \code{image_file}.
#'
#' @examples
#' # load image
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
#'
#' @importFrom magick image_read
#' @importFrom imager magick2cimg
#'
#' @export
image_load <- function(image_file) {

  # load multiple images
  if(length(image_file) > 1) {
    lapply(image_file, function(x) {

      # stop and return error if file doesn't exist
      if(!file.exists(x)) {
        stop(
          paste(x, 'is invalide file path.')
        )
      }

      # load image
      img <- image_read(x)
      magick2cimg(img)
    })
  } else {
    # stop and return error if file doesn't exist
    if(!file.exists(image_file)) {
      stop(
        paste(image_file, 'is invalide file path.')
      )
    }
    # load images
    img <- image_read(image_file)
    magick2cimg(img)
  }
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
