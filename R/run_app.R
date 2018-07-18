#' Run the shiny App
#'
#' @return NULL
#'
#' @importFrom shiny runApp
#'
#' @export
run_app <- function(){
  app_dir <- system.file('colocr_app', package = 'colocr')
  runApp(app_dir, display.mode = 'normal')
}
