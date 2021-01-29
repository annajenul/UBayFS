#' checks whether an object is a UBaymodel
#' @description perform consistency checks of a UBaymodel
#' @param x an object to be checked for class consistency
#' @export

is.UBaymodel <- function(x){
  return(class(x) == "UBaymodel")
}
