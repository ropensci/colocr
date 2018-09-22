context('reproduce_app')

# load required libraries
library(colocr)

test_that('the app results are reproduced faithfuly', {
  # read input and output from the app
  stats <- read.csv(system.file('colocr_app', 'stats_18.09.02_05.15.01.csv', package = 'colocr'))
  inputs <- read.csv(system.file('colocr_app', 'inputs_18.09.02_05.15.08.csv', package = 'colocr'),
                     stringsAsFactors = FALSE)

  # read images
  fls <- lapply(inputs$image, function(x) {
    system.file('extdata', x, package = 'colocr')
  })

  imgs <- image_load(fls)

  # use the app input to the roi_select function
  rep_stats <- imgs %>%
    roi_select(threshold = inputs$threshold,
               shrink = inputs$shrink,
               grow = inputs$grow,
               fill = inputs$fill,
               clean = inputs$clean,
               tolerance = inputs$tolerance,
               n = inputs$roi_num) %>%
    roi_test(type = 'both')

  expect_true(
    all.equal(round(stats$pcc, 2), round(c(rep_stats[[1]]$pcc, rep_stats[[2]]$pcc), 2))
  )
  expect_true(
    all.equal(round(stats$moc, 2), round(c(rep_stats[[1]]$moc, rep_stats[[2]]$moc), 2))
  )
})
