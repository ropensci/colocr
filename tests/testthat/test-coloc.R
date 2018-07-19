context("coloc_test")

test_that("coloc_test works", {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)

  # load images from two channels
  img1 <- load.image(system.file('extdata/', 'Image0001_C002.jpg', package = 'colocr'))
  img2 <- load.image(system.file('extdata/', 'Image0001_C003.jpg', package = 'colocr'))

  # call coloc_test
  corr <- coloc_test(img1, img2, px)

  # test return type
  expect_identical(class(corr), 'list')
  expect_true(corr$p >= -1 && corr$p <= 1)

  # test return both
  corr <- coloc_test(img1, img2, px, type = 'all')

  expect_equal(length(corr), 2)

  # test returns numerics
  corr <- coloc_test(img1, img2, px, num = TRUE)

  expect_equal(length(corr), 4)
  expect_identical(class(corr$channel1), 'numeric')
  expect_identical(class(corr$channel2), 'numeric')
})

test_that('coloc_show works', {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)

  # load images from two channels
  img1 <- load.image(system.file('extdata/', 'Image0001_C002.jpg', package = 'colocr'))
  img2 <- load.image(system.file('extdata/', 'Image0001_C003.jpg', package = 'colocr'))

  # call coloc_test
  corr <- coloc_test(img1, img2, px, num = TRUE)

  # call coloc_show
  p <- coloc_show(corr)

  expect_identical(class(p), c('gg', 'ggplot'))

  # test works with labels
  labs.df <- labels_add(px, n = 3)
  corr <- coloc_test(img1, img2, px, labs.df, num = TRUE)
  p <- coloc_show(corr)

  expect_identical(class(p), c('gg', 'ggplot'))
})
