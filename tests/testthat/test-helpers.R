context("helpers")

test_that("helper functions for coefficients work", {
  set.seed(123)
  r <- rnorm(10)

  set.seed(1234)
  g <- rnorm(10)

  expect_true(round(.pearson(r, g), 3) == round(cor(r, g), 3))
  expect_false(.manders(r, g) == cor(r, g))
})

test_that('.labels_add works', {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)

  # choose parameters
  px <- roi_select(img, threshold = 90)

  # add labels
  labs.px <- .labels_add(px)

  # test return proper data.frame
  expect_true(is.cimg(labs.px))

  # test returns only values in n
  labs.px <- .labels_add(px, n = 3)

  expect_equal(length(unique(as.data.frame(labs.px)$value)), 4)
})
