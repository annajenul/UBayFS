#' @export

loss <- function(state, theta, user.params, log = TRUE){

  dmult <- dmultinom(state, prob = theta, log = log)
  adm <- admissibility(state, A = user.params$constraints$A, b = user.params$constraints$b, rho = user.params$constraints$rho, log = log)

  if(log){
    return(dmult + adm)
  }
  else{
    return(dmult * adm)
  }

}
