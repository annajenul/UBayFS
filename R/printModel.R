#' prints a UBayFS model
#' @description print details of an UBayFS model
#' @param model a UBaymodel object created using build.model
#' @export

print.UBaymodel <- function(model){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  print("data")
  print(str(model$data))
  print(str(model$target))
  print("user.params")
  print(model$user.params)
  print("ensemble.params")
  print(model$ensemble.params)
  print("optim.params")
  print(model$optim.params)
  if(!is.null(model$output)){
    print("MAP")
    print(model$output$map)
  }

}
