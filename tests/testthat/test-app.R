context('app')

test_that("app works", {
  library(shinytest)
  app_dir <- system.file('colocr_app/', package = 'colocr')
  expect_pass(testApp(app_dir, compareImages = FALSE))
})
