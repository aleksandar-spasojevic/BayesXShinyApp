#' runs the ShinyApp
#'
#' @param display.mode see \code{\link{runApp}}
#' @param ... further arguments passed to \code{\link{runApp}}
#'
#' @note arguments send to \code{\link{runApp}}

#' @examples \dontrun{run_app()}
#' @importFrom shiny runApp
#' @export
run_app <- function(display.mode = "normal", ...){
  app_path <- system.file("shiny-app", package = "ShinyApp")
  runApp(app_path, display.mode = display.mode, ...)
}
