#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @param img1 An object of class \code{\link[imager]{cimg}}
#' @param img2 An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param labels A \code{data.frame} of three columns; x, y and value.
#' @param type A \code{character}; "pearson's", "spearman" or "all"
#' @param num A \code{logical}; return the \code{numeric} values of the images
#' or not
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
#' @importFrom purrr map map2
#'
#' @export
coloc_test <- function(img1, img2, px, labels, type = 'pearsons', num = FALSE) {
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

  # use labels to subset images when provided
  if(!missing(labels)) {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[as.cimg(labels)])
    img2.num <- as.numeric(img2.g[as.cimg(labels)])

    # calculate correlations
    ll <- map(list(img1.num, img2.num),
              function(x) split(x, labels$value))
    corr <- switch (type,
                    'pearsons' = list(p = unlist(map2(ll[[1]], ll[[2]], function(x,y) cor(x, y)))),
                    'spearman' = list(r = unlist(map2(ll[[1]], ll[[2]], function(x,y) cor(x, y, method = 'spearman')))),
                    'all' = list(p = unlist(map2(ll[[1]], ll[[2]], function(x,y) cor(x, y))),
                                 r = unlist(map2(ll[[1]], ll[[2]], function(x,y) cor(x, y, method = 'spearman'))))
    )
  } else {
    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[px])
    img2.num <- as.numeric(img2.g[px])

    # calculate correlations
    corr <- switch (type,
                    'pearsons' = list(p = cor(img1.num, img2.num)),
                    'spearman' = list(r = cor(img1.num, img2.num, method = 'spearman')),
                    'all' = list(p = cor(img1.num, img2.num),
                                 r = cor(img1.num, img2.num, method = 'spearman'))
    )
  }

  # add the numeric values when num == TRUE
  if (num) {
    corr$channel1 = img1.num
    corr$channel2 = img2.num
  }
  if(!missing(labels)) {
    corr$labels = labels$value
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
#' @importFrom dplyr bind_cols
#' @importFrom ggplot2 ggplot geom_point aes theme_bw theme labs geom_density
#' @importFrom tidyr gather
#' @importFrom stats setNames
#' @importFrom stats density
#' @importFrom cowplot plot_grid
#'
#' @export
coloc_show <- function(corr) {
  if(!is.list(corr)) {
    stop('corr should be a list, output of coloc_test.')
  }
  if(length(corr) < 3) {
    stop('make sure to call coloc_test with num == TRUE.')
  }

  df <- bind_cols(channel1 = corr$channel1,
                  channel2 = corr$channel2,
                  labels = corr$labels)
  plot_grid(ggplot(df) +
              geom_point(aes(x = channel1, y = channel2, color = as.factor(labels))) +
              theme_bw() +
              theme(legend.position = 'top') +
              labs(x = 'Channel One', y = 'Channel Two',
                   color = 'ROI'),

            df %>%
              setNames(c('One', 'Two', 'labels')) %>%
              gather(channel, value, -labels) %>%
              ggplot() +
              geom_density(aes(value, group = channel, color = channel)) +
              theme_bw() +
              theme(legend.position = 'top') +
              labs(x = 'Value', y = 'Count', color = 'Channel'))
}
