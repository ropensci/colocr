context('reproduce app')

# load required libraries
library(colocr)
library(imager)
library(magick)

test_that('the app results are reproduced faithfuly', {
  # read input and output from the app
  stats <- read.csv(system.file('colocr_app', 'stats_19.05.23_05.55.21.csv', package = 'colocr'))
  inputs <- read.csv(system.file('colocr_app', 'inputs_19.05.23_05.55.22.csv', package = 'colocr'),
                     stringsAsFactors = FALSE)

  # read images
  fls <- lapply(inputs$image, function(x) {
    system.file('extdata', x, package = 'colocr')
  })

  imgs <- image_load(fls)

  # use the app input to the roi_select function
  rep_stats <- imgs %>%
    roi_select(threshold = inputs$threshold,
               shrink = inputs$shrink,
               grow = inputs$grow,
               fill = inputs$fill,
               clean = inputs$clean,
               tolerance = inputs$tolerance,
               n = inputs$roi_num) %>%
    roi_test(type = 'both')

  expect_true(
    all.equal(round(stats$pcc, 3), round(c(rep_stats[[1]]$pcc, rep_stats[[2]]$pcc), 3))
  )
  expect_true(
    all.equal(round(stats$moc, 3), round(c(rep_stats[[1]]$moc, rep_stats[[2]]$moc), 3))
  )
})

test_that('the app results are reproduced using imager only', {
  # read input and output from the app
  stats <- read.csv(system.file('colocr_app', 'stats_19.05.23_05.55.21.csv', package = 'colocr'))
  inputs <- read.csv(system.file('colocr_app', 'inputs_19.05.23_05.55.22.csv', package = 'colocr'),
                     stringsAsFactors = FALSE)

  # read images
  lst <- as.list(inputs)
  num <- list()
  for(i in 1:2) {
    fl <- system.file('extdata', lst$image[i], package = 'colocr')
    img <- image_read(fl)
    img <- magick2cimg(img)

    # transform to gray scale
    img.g <- grayscale(img)

    # apply threshold
    img.t <- threshold(img.g, paste0(lst$threshold[i], '%'))

    # change to pixset
    px <- as.pixset(1-img.t)

    # apply shrink
    px.m <- shrink(px, lst$shrink[i])

    # apply grow
    px.m <- grow(px.m, lst$grow[i])

    # apply fill
    px.m <- fill(px.m, lst$fill[i])

    # apply clean
    px <- clean(px.m, lst$clean[i])

    # add labels
    px.labs <- label(px, tolerance = lst$tolerance[i])
    value <- as.data.frame(px.labs)$value

    ids <- reorder(value, value, length)

    k <- levels(ids)

    k <- k[(length(k)-1):(length(k)-lst$roi_num[i])]

    new.ids <- ifelse(value %in% as.numeric(k), value, 0)
    f <- as.numeric(factor(new.ids))

    new.px <- as.data.frame(px.labs)
    new.px$value <- f - 1
    new.px <- as.cimg(new.px, dim = dim(px))
    labs <- as.data.frame(new.px)$value

    # extract first two channels
    img1.g <- channel(img, ind = 1)
    img2.g <- channel(img, ind = 2)

    # subset and change images to numeric
    img1.num <- as.numeric(img1.g[as.pixset(new.px)])
    img2.num <- as.numeric(img2.g[as.pixset(new.px)])
    num[[i]] <- list(img1.num,
                     img2.num,
                     labs[labs != 0])
  }

  # test pcc
  corr <- list()
  for(i in 1:2) {
    lab <- (num[[i]][[3]])
    res <- list()
    for(j in unique(lab)) {
      c1 <- num[[i]][[1]][lab == j]
      c2 <- num[[i]][[2]][lab == j]

      rx <- c1 - mean(c1)
      gx <- c2 - mean(c2)

      nom <- sum(rx * gx)
      den <- sqrt(sum(rx**2) * sum(gx**2))


      res[[j]] <-nom/den
    }
    corr[[i]] <- res
  }

  expect_true(all.equal(round(stats$pcc, 3), round(unlist(corr), 3)))

  # test moc
  corr <- list()
  for(i in 1:2) {
    lab <- (num[[i]][[3]])
    res <- list()
    for(j in unique(lab)) {
      c1 <- num[[i]][[1]][lab == j]
      c2 <- num[[i]][[2]][lab == j]

      nom <- sum(c1 * c2)
      den <- sqrt(sum(c1**2) * sum(c2**2))

      res[[j]] <- nom/den
    }
    corr[[i]] <- res
  }

  expect_true(all.equal(round(stats$moc, 3), round(unlist(corr), 3)))
})
