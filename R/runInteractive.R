#' @import shiny
#' @export

runInteractive <- function(){
  appDir <- system.file("shinyApp", "RentABayInteractive", package = "RentABay")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `RentABay`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
