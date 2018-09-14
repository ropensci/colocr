library(testthat)
library(shinytest)

test_that("Application works", {
  expect_pass(testApp(".", compareImages = FALSE))
})
