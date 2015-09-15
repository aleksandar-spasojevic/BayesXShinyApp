#' runs ShinyApp
#'
#' @param display.mode see \code{\link{runApp}}
#' @param ... further arguments passed to \code{\link{runApp}}
#'
#' @note arguments send to \code{\link{runApp}}

#' @examples \dontrun{run_app()}
#' @importFrom shiny runApp
#' @export
run_app <- function(display.mode = "normal", which = "shiny-app", ...){
  app_path <- system.file(which, package = "BayesXShinyApp")
  runApp(app_path, display.mode = display.mode, ...)
}
