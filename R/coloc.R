#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param labels An object of class \code{\link[imager]{cimg}}
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
#' px <- parameter_choose(img, threshold = 90)
#'
#' # call coloc_test
#' coloc_test(img, px)
#'
#' @importFrom imager is.cimg is.pixset channel
#' @importFrom stats cor
#'
#' @export
coloc_test <- function(img, px, labels, type = 'pearsons', num = FALSE, ind = c(1,2)) {
  if(!is.cimg(img)) {
    stop('img should be of class cimg.')
  }

  if(!is.pixset(px)) {
    stop('px should be of class pixset.')
  }

  # change images to gray scal
  img1.g <- channel(img, ind = ind[1])
  img2.g <- channel(img, ind = ind[2])

  if(!type %in% c('pearsons', 'manders', 'all')) {
    stop('type takes one of these values; pearsons, manders or all')
  }

  # use labels to subset images when provided
  if(!missing(labels)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[as.pixset(labels)])
    img2.num <- as.numeric(img2.g[as.pixset(labels)])

    # calculate correlations
    f <- as.data.frame(labels)
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
  } else {
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
  if(!missing(labels)) {
    corr$labels = f
  } else if(missing(labels) && num){
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
#' px <- parameter_choose(img, threshold = 90)
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

  # plot
  par(mfrow = c(1,2), mar = c(4,4,1,1))

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
#' @inheritParams .pearson
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
