#' summarizes a UBayFS model
#' @description a summary of an UBayFS model
#' @param model a UBaymodel object created using build.model
#' @export

summary.UBaymodel <- function(model){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  cat(" UBayFS model summary \n",
                    " data: ", paste0(dim(model$data), collapse = "x"), '\n',
                    " labels: ", paste0(levels(model$target), ": ", tabulate(model$target), collapse = " "), "\n\n",
                    " === prior information === \n",
                    paste0(
                      sapply(1:nrow(model$user.params$constraints$A), function(i){
                          paste0(
                            " constraint ",
                            i,
                            ": ( ",
                            paste0(model$user.params$constraints$A[i,], collapse = ","),
                            " ) x <= ",
                            model$user.params$constraints$b[i],
                            "; rho = ",
                            model$user.params$constraints$rho[i]
                          )
                        }),
                      collapse = " \n"
                    ),
                    "\n\n",
                    " === prior weights === \n",
                    " weights: (", paste0(model$user.params$weights, collapse = ","),") \n\n",
                    " === likelihood === \n",
                    " ensemble counts: (", paste0(model$ensemble.params$output$counts, collapse = ","),") \n\n",
                    " === feature selection results === \n",
                    " MAP: ",
                    ifelse(is.null(model$output$map), "no output produced yet", paste0("( ", paste0(model$output$map, collapse = ",")," )")),
                    " \n"
  )


}
