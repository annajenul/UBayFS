#' summarizes a UBayFS model
#' @description a summary of an UBayFS model
#' @param object a UBaymodel object created using build.UBaymodel
#' @param ... unused
#' @export

summary.UBaymodel <- function(object,...){

  if(class(object) != "UBaymodel"){
    stop("Wrong class of object")
  }

  cat(" UBayFS model summary \n",
                    " data: ", paste0(dim(object$data), collapse = "x"), '\n',
                    " labels: ", paste0(levels(object$target), ": ", tabulate(object$target), collapse = " "), "\n\n",
                    " === prior constraints === \n",
                    if(!is.null(object$constraint.params$constraints$A)){
                      paste0(
                        sapply(1:nrow(object$constraint.params$constraints$A), function(i){
                            paste0(
                              " constraint ",
                              i,
                              ": (",
                              paste0(object$constraint.params$constraints$A[i,], collapse = ","),
                              ") x <= ",
                              object$constraint.params$constraints$b[i],
                              "; rho = ",
                              object$constraint.params$constraints$rho[i]
                            )
                          }),
                      collapse = "\n")
                    },
                    "\n",
                    if(!is.null(object$constraint.params$block_constraints$A)){
                       paste0(
                         sapply(1:nrow(object$constraint.params$block_constraints$A), function(i){
                             paste0(
                               " block constraint ",
                               i,
                               ": (",
                               paste0(object$constraint.params$block_constraints$A[i,], collapse = ","),
                               ") x <= ",
                               object$constraint.params$block_constraints$b[i],
                               "; rho = ",
                               object$constraint.params$block_constraints$rho[i]
                             )
                           }),
                        collapse = "\n")
                    }else{"\n"},
                    " \n",
                    " === prior weights === \n",
                    " weights: (", paste0(object$user.params$weights, collapse = ","),") \n\n",
                    " === likelihood === \n",
                    " ensemble counts: (", paste0(object$ensemble.params$output$counts, collapse = ","),") \n\n",
                    " === feature selection results (MAP) === \n",
                    ifelse(is.null(object$output$feature_set), "no output produced yet",
                           paste0(
                              apply(object$output$feature_set, 1, function(x){
                                  paste0(" ( ", paste0(which(x == 1), collapse = ",")," )")
                               }),
                               collapse = "\n")
                          ),
                    " \n"
  )


}
