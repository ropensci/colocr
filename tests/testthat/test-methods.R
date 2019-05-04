context("methods")

# load required libraries
library(imager)
library(magrittr)
library(purrr)

# load images
fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
img <- image_load(fl)

fls <- c(system.file('extdata', 'Image0001_.jpg', package = 'colocr'),
         system.file('extdata', 'Image0003_.jpg', package = 'colocr'))

imgs <- image_load(fls)

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

test_that("roi_select works with multiple images", {
  newimgs <- roi_select(imgs, threshold = 90)
  labels <- map(newimgs, function(x) attr(x, 'label'))

  expect_true(is.list(newimgs))
  expect_true(is.list(labels))
  expect_true(is.cimg(newimgs[[1]]))
  expect_equal(length(unique(labels[[1]])), 2)
})

test_that("roi_select works with multiple images and inputs", {
  newimgs <- roi_select(imgs, threshold = c(90, 95))
  labels <- map(newimgs, function(x) attr(x, 'label'))

  expect_true(is.list(newimgs))
  expect_true(is.list(labels))
  expect_true(is.cimg(newimgs[[1]]))
  expect_equal(length(unique(labels[[1]])), 2)
})

test_that('roi_select handels threshold errors', {
  expect_error(roi_select(img, '90'))
  expect_error(roi_select(img, 100))
  expect_error(roi_select(img, -1))
  expect_warning(roi_select(imgs, threshold = c(90, 91, 93)))
})

test_that("roi_show works.", {
  g <- roi_select(img, threshold = 90) %>%
    roi_show()

  expect_true(is.null(g))
})

test_that("roi_show works with mulitple images", {
  g <- roi_select(imgs, threshold = 90) %>%
    roi_show()

  expect_true(is.null(g))
})

test_that("roi_show works with mulitple images and inputs", {
  g <- roi_select(imgs, threshold = 90) %>%
    roi_show(ind = list(c(1,2), c(1,2)))

  expect_true(is.null(g))
})

test_that("roi_check works.", {
  g <- roi_select(img, threshold = 90) %>%
    roi_check()

  expect_true(is.null(g))
})

test_that("roi_check works with mulitple images", {
  g <- roi_select(imgs, threshold = 90) %>%
    roi_check()

  expect_true(is.null(g))
})

test_that("roi_check works with mulitple images and inputs", {
  g <- roi_select(imgs, threshold = 90) %>%
    roi_check(ind = list(c(1,2), c(1,2)))

  expect_true(is.null(g))
})

test_that("roi_test works.", {
  res <- roi_select(img, threshold = 90) %>%
    roi_test()

  expect_true(is.data.frame(res))
  expect_equal(length(res$pcc), 1)
})

test_that("roi_test works with multiple images", {
  res <- roi_select(imgs, threshold = 90) %>%
    roi_test()

  expect_true(is.list(res))
  expect_equal(length(res), length(imgs))
  expect_equal(length(res[[1]]$pcc), 1)
})

test_that("roi_test works with multiple images and inputs", {
  res <- roi_select(imgs, threshold = 90, n = 3) %>%
    roi_test(ind = list(c(1,2), c(1,2)), type = 'both')

  expect_true(is.list(res))
  expect_equal(length(res), length(imgs))

  expect_true(is.data.frame(res[[1]]))
  expect_equal(ncol(res[[1]]), 2)
  expect_equal(nrow(res[[1]]), 3)
})
