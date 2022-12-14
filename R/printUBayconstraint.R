#' Prints the `UBayconstraint` object
#' @param x a `UBayconstraint` object
#' @importFrom methods is
#' @export
print.UBayconstraint <- function(x,...){

  if(!is(x, "UBayconstraint")){
    stop("Wrong class of x")
  }

  print(x$A)
  print(x$b)
  print(x$rho)
  print(x$block_matrix)
}

#' @describeIn print.UBayconstraint Prints a summary of the `UBayconstraint` object
#' @param object a `UBayconstraint` object
#' @importFrom methods is
#' @export
summary.UBayconstraint <- function(object,...){

  if(!is(object, "UBayconstraint")){
    stop("Wrong class of object")
  }

  cat(
    paste0(
      sapply(1:nrow(object$A), function(i){
        paste0(
          " constraint ",
          i,
          ": (",
          paste0(object$A[i,], collapse = ","),
          ") x <= ",
          object$b[i],
          "; rho = ",
          object$rho[i]
        )
      }),
      collapse = "\n"))
}
