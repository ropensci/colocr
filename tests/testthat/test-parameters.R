context("parameter_choose.R")

test_that("test parameter_choose", {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)

  # returns the correct object
  expect_true(is.pixset(px))

  # pixset has same dimensions as img.g
  expect_equal(dim(img.g), dim(px))
})

test_that('parameter_show works', {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)

  # call parameter_show
  p <- parameter_show(img, px)

  expect_identical(class(p), 'list')
})

test_that('parameter_show works with labels', {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)
  labs.df <- labels_add(px, n = 3)

  # call parameter_show
  p <- parameter_show(img, px, labs.df)

  expect_identical(class(p), 'list')
})

#test_that('function works properly in app', {
#  app_dir <- system.file('colocr_app', package = 'colocr')
#  shinytest::testApp(app_dir)
#})

test_that('labels_add works', {
  # load libraries
  library(imager)

  # load images
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- load.image(fl)
  img.g <- grayscale(img)

  # choose parameters
  px <- parameter_choose(img.g, threshold = 90)

  # add labels
  labs.px <- labels_add(px)

  # test return proper data.frame
  expect_true(is.cimg(labs.px))

  # test returns only values in n
  labs.px <- labels_add(px, n = 3)

  expect_equal(length(unique(as.data.frame(labs.px)$value)), 4)
})
