#' \code{colocr} package
#'
#' Conduct Co-localization Analysis of Microscopy Images
#'
#' @docType package
#' @name colocr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## fix by @jennybc
## source https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1")  utils::globalVariables(c('value',
                                                        'channel1',
                                                        'channel2'))
