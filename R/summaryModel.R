#' summarizes a UBayFS model
#' @description a summary of an UBayFS model
#' @param object a UBaymodel object created using build.model
#' @param ... unused
#' @export

summary.UBaymodel <- function(object,...){

  if(class(object) != "UBaymodel"){
    stop("Wrong class of object")
  }

  cat(" UBayFS model summary \n",
                    " data: ", paste0(dim(object$data), collapse = "x"), '\n',
                    " labels: ", paste0(levels(object$target), ": ", tabulate(object$target), collapse = " "), "\n\n",
                    " === prior information === \n",
                    paste0(
                      sapply(1:nrow(object$user.params$constraints$A), function(i){
                          paste0(
                            " constraint ",
                            i,
                            ": ( ",
                            paste0(object$user.params$constraints$A[i,], collapse = ","),
                            " ) x <= ",
                            object$user.params$constraints$b[i],
                            "; rho = ",
                            object$user.params$constraints$rho[i]
                          )
                        }),
                      collapse = " \n"
                    ),
                    "\n\n",
                    " === prior weights === \n",
                    " weights: (", paste0(object$user.params$weights, collapse = ","),") \n\n",
                    " === likelihood === \n",
                    " ensemble counts: (", paste0(object$ensemble.params$output$counts, collapse = ","),") \n\n",
                    " === feature selection results === \n",
                    " MAP: ",
                    ifelse(is.null(object$output$map), "no output produced yet", paste0("( ", paste0(object$output$map, collapse = ",")," )")),
                    " \n"
  )


}
