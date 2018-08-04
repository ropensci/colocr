context("methods")

# load libraries
library(imager)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- load.image(fl)

# choose parameters
px <- roi_select(img, threshold = 90)

test_that("test roi_select", {
  # returns the correct object type
  expect_true(is.pixset(px))

  # pixset has same dimensions as img
  expect_equal(dim(img)[1:2], dim(px)[1:2])
})

# show parameters
p <- roi_show(img, px)

test_that('roi_show works', {
  # returns the correct object type
  expect_identical(class(p), 'list')
})

# add labels
labs.px <- roi_select(img, threshold = 90, n = 3)

test_that('roi_show works with labels', {
  # call roi_show
  p <- roi_show(img, px, labs.px)

  expect_identical(class(p), 'list')
})

# call coloc_test
corr <- coloc_test(img, px)
corr2 <- coloc_test(img, px, type = 'all')
corr3 <- coloc_test(img, px, num = TRUE)
corr4 <- coloc_test(img, labs.px, num = TRUE)
corr5 <- coloc_test(img, labs.px, type = 'all', num = TRUE)

test_that("coloc_test works", {
  # test return type
  expect_identical(class(corr), 'list')
  expect_true(corr$p >= -1 && corr$p <= 1)

  # test return both
  expect_equal(length(corr2), 2)

  # test returns numerics
  expect_equal(length(corr3), 4)
  expect_identical(class(corr3$channel1), 'numeric')
  expect_identical(class(corr3$channel2), 'numeric')
})


test_that("coloc_test works with labels", {
  # test return type
  expect_identical(class(corr5), 'list')
  expect_true(length(corr5$p) == length(unique(corr5$labels)))
})

test_that('coloc_show works', {
  # call coloc_show
  p <- coloc_show(corr3)

  expect_true(is.null(p))

  p <- coloc_show(corr4)

  expect_true(is.null(p))
})

test_that('coloc_show works with labels', {
  p <- coloc_show(corr5)

  expect_true(is.null(p))
})
