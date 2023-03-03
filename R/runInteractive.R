#' Run an interactive Shiny app for demonstration
#' @description Starts an interactive R Shiny application in the browser.
#' @import shiny
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @importFrom utils select.list
#' @return calls Shiny app, no return value
#' @export

runInteractive = function(){
  req_packages <- c(caret = 'caret',
                   GSelection = 'GSelection',
                   glmnet = 'glmnet',
                   rpart = 'rpart',
                   shinyalert = 'shinyalert',
                   DT = 'DT',
                   RColorBrewer = 'RColorBrewer',
                   shinyWidgets = 'shinyWidgets',
                   shinyjs = 'shinyjs',
                   shinyBS = 'shinyBS',
                   randomForest = 'randomForest'
                   )
  missing_packages = c()
  for (package in req_packages) {
    if (!require(package, quietly = TRUE, character.only = TRUE)){
      missing_packages = c(missing_packages, package)
    }
  }


  if(length(missing_packages) > 0){
    stop(paste0('Please install missing packages: ', paste(missing_packages, collapse = ', ')))
  }



  appDir <- system.file('shinyApp', 'UBayFSInteractive', package = 'UBayFS')
  if (appDir == '') {
   stop('Could not find app directory. Try re-installing `UBayFS`.', call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = 'normal',
               launch.browser = TRUE)
}
