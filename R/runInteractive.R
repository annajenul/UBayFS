#' runs an interactive shiny app for demonstration
#' @description starts a shiny application in the browser
#' @import shiny
#' @export

runInteractive = function(){
  appDir <- system.file("shinyApp", "UBayFSInteractive", package = "UBayFS")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `UBayFS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
