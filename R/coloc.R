#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @param img An object of class \code{\link[imager]{cimg}}
#' @param px An object of class \code{\link[imager]{pixset}}
#' @param labels An object of class \code{\link[imager]{cimg}}
#' @param type A \code{character}; "pearson's", "spearman" or "all"
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
#' img.g <- grayscale(img)
#'
#' # choose parameters
#' px <- parameter_choose(img.g, threshold = 90)
#'
#' # call coloc_test
#' coloc_test(img, px)
#'
#' @importFrom imager is.cimg is.pixset channel
#' @importFrom stats cor
#' @importFrom purrr map map2
#' @importFrom dplyr filter pull
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
    img1.num <- as.numeric(img1.g[as.pixset(labels)])
    img2.num <- as.numeric(img2.g[as.pixset(labels)])

    # calculate correlations
    f <- as.data.frame(labels) %>%
      filter(value != 0) %>%
      pull(value)

    ll <- map(list(img1.num, img2.num),
              function(x) split(x, f))
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
#' img.g <- grayscale(img)
#'
#' # choose parameters
#' px <- parameter_choose(img.g, threshold = 90)
#'
#' # call coloc_test
#' corr <- coloc_test(img, px, num = TRUE)
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
