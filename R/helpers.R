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
.labels_add <- function(px, tolerance = .1, n = 1) {
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
.pearson <- function(r, g) {
  rx <- r - mean(r)
  gx <- g - mean(g)

  nom <- sum(rx * gx)
  den <- sqrt(sum(rx**2) * sum(gx**2))
  nom/den
}

#' Calculate Marnders Overlap Coefficient
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
.manders <- function(r, g) {
  nom <- sum(r * g)
  den <- sqrt(sum(r**2) * sum(g**2))
  nom/den
}
