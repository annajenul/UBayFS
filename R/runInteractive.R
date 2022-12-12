#' Run an interactive Shiny app for demonstration
#' @description Starts an interactive R Shiny application in the browser.
#' @import shiny
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @importFrom utils select.list
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
                   shinyBS = "shinyBS" %in% installed_packages,
                   randomForest = "randomForest" %in% installed_packages
                   )

  if(!all(req_packages)){
    missing_packages <- names(req_packages)[!req_packages]
    inst = "yes"
    inst <- select.list(title = paste0("The shiny application requires the following packages: \n", paste(missing_packages, collapse = ","), "\nDo you want to install these packages and run the GUI?"),
                        choices = c("yes", "no"))
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
