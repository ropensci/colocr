#' Test colocalization
#'
#' @param img1 An object of class cimg
#' @param img2 An object of class cimg
#' @param px An object of class pixset
#' @param type A character; pearson's, spearman or all
#' @param num A logical; return the numeric values of the images or not.
#'
#' @return A list of one or two correlations measures and two numerics when num
#' is TRUE
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
#' coloc_test(img1, img2, px)
#'
#' @importFrom imager is.cimg is.pixset grayscale
#' @importFrom stats cor
#'
#' @export
coloc_test <- function(img1, img2, px, type = 'pearsons', num = FALSE) {
  if(!is.cimg(img1)) {
    stop('img1 should be of class cimg.')
  }
  if(!is.cimg(img2)) {
    stop('img2 should be of class cimg.')
  }
  if(all(dim(img2) != dim(img2))) {
    stop('img1 and img2 do not have identical dimensions.')
  }
  if(!is.pixset(px)) {
    stop('px should be of class pixset.')
  }

  # change images to gray scal
  img1.g <- grayscale(img1)
  img2.g <- grayscale(img1)

  if(all(dim(img1.g) != dim(px))) {
    stop('dimensions of px should match dimensions of img1 in grayscale.')
  }
  if(all(dim(img2.g) != dim(px))) {
    stop('dimensions of px should match dimensions of img2 in grayscale.')
  }

  if(!type %in% c('pearsons', 'spearman', 'all')) {
    stop('type takes one of these values; pearsons, spearman or all')
  }

  # subset and change images to numeric
  img1.num <- as.numeric(img1[px])
  img2.num <- as.numeric(img2[px])

  # calculate correlations
  corr <- switch (type,
    'pearsons' = list(p = cor(img1.num, img2.num)),
    'spearman' = list(r = cor(img1.num, img2.num, method = 'spearman')),
    'all' = list(p = cor(img1.num, img2.num),
                 r = cor(img1.num, img2.num, method = 'spearman'))
  )

  # add the numeric values when num == TRUE
  if (num) {
    corr$channel1 = img1.num
    corr$channel2 = img2.num
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
#' corr <- coloc_test(img1, img2, px, num = TRUE)
#'
#' # call coloc_show
#' coloc_show(corr)
#'
#' @importFrom graphics par plot lines
#' @importFrom stats density
#'
#' @export
coloc_show <- function(corr) {
  if(!is.list(corr)) {
    stop('corr should be a list, output of coloc_test.')
  }
  if(length(corr) < 3) {
    stop('make sure to call coloc_test with num == TRUE.')
  }
  par(mfrow = c(1,2))
  plot(corr$channel1,
       corr$channel2,
       xlab = 'Channel One', ylab = 'Channel Two', main = '')

  plot(density(corr$channel1), col = 'blue', main = '')
  lines(density(corr$channel2), col = 'red')
}
