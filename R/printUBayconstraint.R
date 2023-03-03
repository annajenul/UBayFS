#' Prints the `UBayconstraint` object
#' @param x a `UBayconstraint` object
#' @param ... additional print parameters
#' @importFrom methods is
#' @return prints model summary to the console, no return value
#' @export
print.UBayconstraint <- function(x,...){

  if(!is(x, 'UBayconstraint')){
    stop('Wrong class of x')
  }
  cat(' A\n')
  print(x$A)
  cat(' b\n')
  print(x$b)
  cat(' rho\n')
  print(x$rho)
  cat(' block_matrix\n')
  print(x$block_matrix)
}

#' @describeIn print.UBayconstraint Prints a summary of the `UBayconstraint` object
#' @param object a `UBayconstraint` object
#' @importFrom methods is
#' @export
summary.UBayconstraint <- function(object,...){

  if(!is(object, 'UBayconstraint')){
    stop('Wrong class of object')
  }

  block_constraint = ifelse(identical(object$block_matrix, diag(nrow = ncol(object$block_matrix))), FALSE, TRUE)

  if(block_constraint){
    cat(' block constraints with ', nrow(object$block_matrix), 'blocks\n')
  }

  if(!is.null(object$A)){
    cat(paste0(
      sapply(1:nrow(object$A), function(i){
        paste0(
          ifelse(block_constraint, ' block', ''),
          ' constraint ',
          i,
          ': (',
          paste0(object$A[i,], collapse = ','),
          ') x <= ',
          object$b[i],
          '; rho = ',
          object$rho[i]
        )
      }),
      collapse = '\n'), '\n')
  }
}
