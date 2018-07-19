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

  # load images from two channels
  img1 <- load.image(system.file('extdata/', 'Image0001_C002.jpg', package = 'colocr'))
  img2 <- load.image(system.file('extdata/', 'Image0001_C003.jpg', package = 'colocr'))

  # call parameter_show
  p <- parameter_show(img, img1, img2, px)

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
  labs.df <- labels_add(px)

  # test return proper data.frame
  expect_s3_class(labs.df, 'data.frame')
  expect_identical(names(labs.df), c('value', 'x', 'y'))

  # test returns only values in n
  labs.df <- labels_add(px, n = 3)

  expect_equal(length(unique(labs.df$value)), 3)
})
