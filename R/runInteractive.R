#' Run an interactive shiny app for demonstration
#' @description Start a shiny application in the browser.
#' @import shiny
#' @export

runInteractive = function(){

  if(!require("caret")){stop("For this application, the caret package is required.")}
  if(!require("GSelection")){stop("For this application, the GSelection package is required.")}
  if(!require("glmnet")){stop("For this application, the glmnet package is required.")}
  if(!require("rpart")){stop("For this application, the rpart package is required.")}
  appDir <- system.file("shinyApp", "UBayFSInteractive", package = "UBayFS")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `UBayFS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal",
                launch.browser = TRUE)
}
