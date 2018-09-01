context("pipeable")

# load required libraries
library(imager)
library(magrittr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- load.image(fl)

test_that("roi_select works", {
  newimg <- roi_select(img, threshold = 90)
  labels <- attr(newimg, 'label')

  expect_true(is.cimg(newimg))
  expect_equal(length(unique(labels)), 2)
})

test_that("roi_select works with labels", {
  newimg <- roi_select(img, threshold = 90, n = 2)
  labels <- attr(newimg, 'label')

  expect_true(is.cimg(newimg))
  expect_equal(length(unique(labels)), 3)
})

test_that("roi_check works.", {
  g <- roi_select(img, threshold = 90) %>%
    roi_check()
  class(g)
  expect_true(is.null(g))
})

test_that("roi_test works.", {
  res <- roi_select(img, threshold = 90) %>%
    roi_test()

  expect_true(is.list(res))
})
