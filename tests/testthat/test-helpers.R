context("helpers")

library(imager)

test_that("image_load works", {
  fl <- system.file('extdata', 'Image0001_.jpg', package = 'colocr')
  img <- image_load(fl)

  expect_true(is.cimg(img))
})

test_that('image_load works with mulitple files.', {
  fls <- list.files(system.file('extdata', package = 'colocr'),
                    full.names = TRUE)
  imgs <- image_load(fls)
  expect_true(is.list(imgs))
  expect_equal(length(fls), length(imgs))
  expect_true(is.cimg(imgs[[1]]))
})

test_that("image_load handels errors", {
  fl <- system.file('extdata', 'missing_file.jpg', package = 'colocr')
  expect_error(image_load(fl))

  fls <- c(system.file('extdata', 'missing_file.jpg', package = 'colocr'),
           system.file('extdata', 'Image0001_.jpg', package = 'colocr'))
  expect_error(image_load(fls))
})
