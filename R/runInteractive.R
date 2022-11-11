#' Run an interactive shiny app for demonstration
#' @description Start a shiny application in the browser.
#' @import shiny
#' @import tcltk
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @export

runInteractive = function(){

  installed_packages = installed.packages()
  req_packages <- c(caret = "caret" %in% installed_packages,
                   GSelection = "GSelection" %in% installed_packages,
                   glmnet = "glmnet" %in% installed_packages,
                   rpart = "rpart" %in% installed_packages,
                   shinyalert = "shinyalert" %in% installed_packages,
                   DT = "DT" %in% installed_packages,
                   RColorBrewer = "RColorBrewer" %in% installed_packages,
                   shinyWidgets = "shinyWidgets" %in% installed_packages,
                   shinyjs = "shinyjs" %in% installed_packages,
                   shinyBS = "shinyBS" %in% installed_packages
                   )

  if(!all(req_packages)){
    missing_packages <- names(req_packages)[!req_packages]
    inst <- as.character(tcltk::tkmessageBox(message = paste("The shiny application requires the following packages: \n", paste(missing_packages, collapse = ","), "\nDo you want to install these packages and run the GUI?"), type=c("yesno")))
    if(inst == "yes"){
      install.packages(missing_packages)
    }
    else{
      return()
    }
  }

  appDir <- system.file("shinyApp", "UBayFSInteractive", package = "UBayFS")
  if (appDir == "") {
   stop("Could not find app directory. Try re-installing `UBayFS`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal",
               launch.browser = TRUE)
}
