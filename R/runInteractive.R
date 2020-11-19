#' @import shiny
#' @export

runInteractive <- function(){
  appDir <- system.file("shinyApp", "UBayInteractive", package = "UBay")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `UBay`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
