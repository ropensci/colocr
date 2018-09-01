context('test app')

library(shinytest)

test_that("app works", {
  app_dir <- system.file('colocr_app/', package = 'colocr')
  expect_pass(testApp(app_dir, compareImages = FALSE))
})
