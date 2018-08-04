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

# call intensity_get
pix_int <- intensity_get(img, px)
pix_int2 <- intensity_get(img, labs.px)

test_that("intensity_get works", {
  # returns correct type
  expect_true(is.list(pix_int))
  expect_equal(length(pix_int), 3)
})

test_that("intensity_get works with labels", {
  # retursn correct type
  expect_true(is.list(pix_int2))
  expect_equal(length(pix_int2), 3)
  expect_equal(length(unique(pix_int2[[3]])), 3)
})

# call intensity_show
test_that('intensity_show works', {
  # call intensity_show
  p <- intensity_show(pix_int)

  expect_true(is.null(p))
})

test_that('intensity_show works with labels', {
  p <- intensity_show(pix_int2)

  expect_true(is.null(p))
})

# call coloc_test
corr <- coloc_test(pix_int, type = 'pearsons')
corr2 <- coloc_test(pix_int, type = 'manders')
corr3 <- coloc_test(pix_int, type = 'all')
corr4 <- coloc_test(pix_int2, type = 'all')

test_that('coloc_test works', {
  # return correct type
  expect_true(is.list(corr))
  expect_equal(length(corr), 1)
  expect_true(all(corr$p < 1, corr$p > -1))

  expect_true(is.list(corr2))
  expect_equal(length(corr2), 1)
  expect_true(all(corr2$r < 1))

  expect_true(is.list(corr3))
  expect_equal(length(corr3), 2)
})

test_that('coloc_test works with labels', {
  # return correct type
  expect_true(is.list(corr4))
  expect_equal(length(corr4), 2)
  expect_true(length(corr4$p) == length(corr4$r))
})
