#' Select regions of interest
#'
#' Select regions of interest in an image using different morphological operations
#'
#' @param img An object of class \code{\link[imager]{cimg}} or a \code{list} of
#' multiple \code{\link[imager]{cimg}} items
#' @param threshold A \code{numeric} to be passed to
#' \code{\link[imager]{threshold}} or a \code{vector} of values for each
#' image in \code{img}
#' @param shrink A \code{numeric} to be passed to \code{\link[imager]{shrink}}
#' or a \code{vector} of values for each image in \code{img}
#' @param grow A \code{numeric} to be passed to \code{\link[imager]{grow}}
#' or a \code{vector} of values for each image in \code{img}
#' @param fill A \code{numeric} to be passed to \code{\link[imager]{fill}}
#' or a \code{vector} of values for each image in \code{img}
#' @param clean A \code{numeric} to be passed to \code{\link[imager]{clean}}
#' or a \code{vector} of values for each image in \code{img}
#' @param tolerance A \code{numeric} to be passed to \code{\link[imager]{label}}
#' or a \code{vector} of values for each image in \code{img}
#' @param n A \code{numeric} of the number of regions of interest
#' or a \code{vector} of values for each image in \code{img}
#'
#' @return A \code{\link[imager]{cimg}}. The original input \code{img} with an
#' additional attribute \code{label}. \code{label} is a \code{vector} of
#' \code{integer}s. The labels for the selected regions of interests starts
#' from 1 and 0 is ignored. When \code{img} is a list, a \code{list} is
#' returned.
#'
#' @details The function applies several \code{\link{imager}} morphological
#' manipulations to select the regions of interest. These include
#' \code{\link[imager]{threshold}} which sets all values below certain cut to
#' 0; \code{\link[imager]{shrink}}/\code{\link[imager]{grow}} for pixel set
#' dilation and erosion; \code{\link[imager]{fill}}/\code{\link[imager]{clean}}
#' for removing isolated regions and holes. When \code{n} is provided, the
#' individual regions (connected components) are selected where \code{tolerance}
#' is used to determine if two pixels belong to the same region.
#'
#' @examples
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
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
                             clean = 5, tolerance = .1, n = 1) {
  UseMethod('roi_select')
}

#' @export
roi_select.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg or a list of cimg objects."))
}

#' @export
roi_select.cimg <- function(img, threshold = 50, shrink = 5, grow = 5, fill = 5,
                            clean = 5, tolerance = .1, n = 1) {

  # check valid input
  if(!missing(threshold) & !is.numeric(threshold)) {
    stop('threshold should be a numeric >= 0 and < 100.')
  }
  if(!missing(threshold) & (threshold >= 100 | threshold < 0)) {
    stop('threshold should be a numeric >= 0 and < 100.')
  }

  # Ideally, I'd like to check type and value of other arguments,
  # however, I currently cannot since these arguments are optional

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
  labs.px <- .labels_add(px.m, tolerance = tolerance, n = n)
  attr(img, 'label') <- as.numeric(labs.px)

  # return object
  return(img)
}

#' @export
roi_select.list <- function(img, threshold, shrink = 5, grow = 5, fill = 5,
                              clean = 5, tolerance = .1, n = 1) {
  # get the length of the image list
  img_n <- length(img)

  # repeat arguments to match list length
  inputs <- list(threshold = threshold,
                 shrink = shrink,
                 grow = grow,
                 fill = fill,
                 clean = clean,
                 tolerance = tolerance,
                 n = n)

  for(i in seq_along(inputs)) {
    # lenght of argument
    input_n <- length(inputs[[i]])

    # use first item and return warning if not a single value or doesn't match
    # length of image list
    if(input_n != img_n & input_n != 1) {
      inputs[[i]] <- inputs[[i]][1]
      warning(paste0("Only first value in ", names(inputs)[[i]], ' will be used.'))
    }

    # match length of the arguments to that of the list of images
    if(input_n != img_n) {
      inputs[[i]] <- rep(inputs[[i]], img_n)
    }
  }

  # loop over the list of images and call roi_select
  newimgs <- list()
  for(i in 1:img_n) {
      newimgs[[i]] <- roi_select(img[[i]],
                            threshold = inputs$threshold[i],
                            shrink = inputs$shrink[i],
                            grow = inputs$grow[i],
                            fill = inputs$fill[i],
                            clean = inputs$clean[i],
                            tolerance = inputs$tolerance[i],
                            n = inputs$n[i])
  }

  # return list of images
  return(newimgs)
  }

#' Show the selected regions of interest
#'
#' Show/highlight the selected regions of interest on different image channels
#'
#' @param img A \code{\link[imager]{cimg}} object or a \code{list} of multiple
#' images such as the one returned from \code{\link{roi_select}}
#' @param ind A \code{numeric} object of length two. For the channel indexes.
#' or a \code{list} of similar vectors for each of \code{img} items.
#'
#' @return NULL
#'
#' @details calling this function with \code{img} object which is returned from
#' \code{\link{roi_select}} returns four different plots. The original image, a
#' low resolution representation of the selected regions of interest and the
#' two channels indicated through \code{ind} highlighted.
#'
#' @examples
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
#'
#' # choose and show ROI
#' oldpar <- par()
#' par(mfrow=c(2,2))
#'
#' roi_select(img, threshold = 90) %>%
#'   roi_show()
#'
#' par(oldpar)
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
                ". img should be a cimg or a list of cimg objects."))
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

  # return null
  invisible(NULL)
}

#' @export
roi_show.list <- function(img, ind = c(1,2)) {

  # get the length of the image list
  img_n <- length(img)

  # repeat argument to match list length
  if(!is.list(ind)) {
    ind <- rep(list(ind), img_n)
  }

  # loop over the images of lists and call roi_show
  for(i in 1:img_n){
    roi_show(img[[i]],
             ind = ind[[i]])
  }

  # return null
  invisible(NULL)
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
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
#'
#' # choose ROI and show the pixel intensities
#' oldpar <- par()
#' par(mfrow = c(1, 2))
#'
#' roi_select(img, threshold = 90) %>%
#'   roi_check()
#'
#' par(oldpar)
#'
#' @importFrom stats density
#' @importFrom scales alpha
#' @importFrom graphics plot lines
#'
#' @export
roi_check <- function(img, ind = c(1,2)) {
  UseMethod('roi_check')
}

#' @export
roi_check.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                ". img should be a cimg or a list of cimg objects."))
}

#' @export
roi_check.cimg <- function(img, ind = c(1,2)) {
  # get pixel intensities
  pix_int <- .intensity_get(img, ind = ind)

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

  # return null
  invisible(NULL)
}

#' @export
roi_check.list <- function(img, ind = c(1,2)) {
  # get the length of the image list
  img_n <- length(img)

  # repeat argument to match list length
  if(!is.list(ind)) {
    ind <- rep(list(ind), img_n)
  }

  # loop over the images of lists and call roi_check
  for(i in 1:img_n) {
    roi_check(img[[i]],
              ind = ind[[i]])
  }

  # return null
  invisible(NULL)
}

#' Test Co-localization
#'
#' Perform co-localization test statistics.
#'
#' @inheritParams roi_show
#' @param type A \code{character} vector of the desired co-localization
#' statistics. Default is 'pcc', other inputs are 'moc' or 'both'.
#'
#' @return A \code{data.frame} or a \code{list} of \code{data.frame}s.
#'
#' @details The co-localization stats requested in \code{type} is returned as
#' a column for each. When different labels are provided, the stats are
#' calculated for each label individually. When is \code{img} is a \code{list}
#' a \code{list} of such \code{data.frame}s is returned
#'
#' @examples
#' # load images
#' fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
#' img <- image_load(fl)
#'
#' # choose roi and test colocalization
#' roi_select(img, threshold = 90) %>%
#'   roi_test()
#'
#' @importFrom imager is.cimg is.pixset channel
#'
#' @export
roi_test <- function(img, ind = c(1,2), type = 'pcc') {
  UseMethod('roi_test')
}

#' @export
roi_test.default <- function(img, ...) {
  warning(paste("img is of class",
                class(img),
                "img should be a cimg or a list of cimg objects."))
}

#' @export
roi_test.cimg <- function(img, ind = c(1,2), type = 'pcc') {
  # get pixel intensity
  pix_int <- .intensity_get(img, ind = ind)

  if(!type %in% c('pcc', 'moc', 'both')) {
    stop('type takes one of these values; pcc, moc or both')
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
                    'pcc' = data.frame(pcc = unlist(mapply(function(x, y) {
                      .pearson(x, y)
                      }, ll[[1]], ll[[2]]))),
                    'moc' = data.frame(moc = unlist(mapply(function(x, y) {
                      .manders(x, y)
                      }, ll[[1]], ll[[2]]))),
                    'both' = data.frame(pcc = unlist(mapply(function(x, y) {
                      .pearson(x, y)
                      }, ll[[1]], ll[[2]])),
                                 moc = unlist(mapply(function(x, y) {
                                   .manders(x, y)
                                   }, ll[[1]], ll[[2]])))
    )
  } else {
    # calculate correlations
    corr <- switch (type,
                    'pcc' = data.frame(pcc = .pearson(img1.num, img2.num)),
                    'moc' = data.frame(moc = .manders(img1.num, img2.num)),
                    'both' = data.frame(pcc = .pearson(img1.num, img2.num),
                                 moc = .manders(img1.num, img2.num))
    )
  }

  # retrun corr
  return(corr)
}

#' @export
roi_test.list <- function(img, ind = c(1,2), type = 'pcc') {
  # get the length of the image list
  img_n <- length(img)

  # repeat arguments to match list length
  if(!is.list(ind)) {
    ind <- rep(list(ind), img_n)
  }

  # loop over the images of lists and call roi_check
  tst <- list()
  for(i in 1:img_n) {
    tst[[i]] <- roi_test(img[[i]],
                         ind = ind[[i]],
                         type = type)
  }

  # return list
  return(tst)
}

#' Run the shiny App
#'
#' @return NULL
#'
#' @importFrom shiny runApp
#'
#' @export
colocr_app <- function(){
  app_dir <- system.file('colocr_app', package = 'colocr')
  runApp(app_dir, display.mode = 'normal')
}
