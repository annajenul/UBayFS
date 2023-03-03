#' Build a constraint system
#' @description Build an inequation system from constraints provided by the user.
#' @details The function transforms user information about relations between features (must-link or cannot-link constraints) and maximum feature set size (max-size) into a linear inequation system. In addition, the relaxation parameter `rho` can be specified to achieve soft constraints.
#' @param constraint_types a vector of strings denoting the type of constraint to be added; options: `max_size`, `must_link`, `cannot_link`
#' @param constraint_vars a list of parameters defining the constraints; in case of max-size constraints, the list element must contain an integer denoting the maximum size of the feature set, in case of max-link or cannot link, the list element must be a vector of feature indices to be linked
#' @param num_elements the total number of features (feature-wise constraints) or blocks (block-wise constraints) in the dataset
#' @param rho a positive parameter denoting the level of relaxation; `Inf` denotes a hard constraint, i.e. no relaxation
#' @param block_list the list of feature indices for each block; only required, if block-wise constraints are built and `block_matrix` is `NULL`
#' @param block_matrix the matrix containing affiliations of features to each block; only required, if block-wise constraints are built and `block_list` is `NULL`
#' @return a `UBayconstraint` containing a matrix `A` and a vector `b` representing the inequality system `Ax<=b`, and a vector `rho` representing the penalty shape
#' @examples
#' # given a dataset with 10 features, we create a max-size constraint limiting
#' # the set to 5 features and a cannot-link constraint between features 1 and 2
#' buildConstraints(constraint_types = c('max_size','cannot_link'),
#'                  constraint_vars = list(5, c(1,2)),
#'                  num_elements = 10,
#'                  rho = 1)
#' @export

buildConstraints = function(constraint_types, constraint_vars, num_elements, rho = 1, block_list = NULL, block_matrix = NULL){

  # check input
  if(!all(constraint_types %in% c('max_size', 'must_link', 'cannot_link'))){
    stop('Error: wrong constraint type provided')
  }
  if(length(constraint_types) != length(constraint_vars)){
    stop('Error: constraint_vars must have same length as constraint_types')
  }
  if(num_elements <= 0 | num_elements %%1 != 0){
    stop('Error: num_elements must be a positive integer')
  }
  if(any(rho <= 0)){
    stop('Error: rho must be positive')
  }
  if(length(rho) < 1){
    stop('Error: rho must have positive length')
  }
  else if(length(rho) == 1){
    rho = rep(rho, length(constraint_types))
  }
  else if((length(rho) > 1) & (length(rho) != length(constraint_types))){
    stop('Error: rho has wrong length')
  }

  # define constraints
  max_size = function(smax, num_feats){
    return(list(A = matrix(1, nrow = 1,
                           ncol = num_feats),
                b = smax))
  }

  must_link = function(sel, num_feats){
    if(length(sel) > 1){
      pairs = as.matrix(expand.grid(sel, sel)) 					# all pairs
      pairs = pairs[									# delete main diagonal (pair of two identical features)
        -which(pairs[,1] == pairs[,2]),
      ]
      newA = t(apply(pairs, 1,
                     function(x){return(
                       ((1:num_feats) == x[1]) - ((1:num_feats) == x[2])
                     )}))
      return(list(A = newA,
                  b = rep(0, nrow(newA))
      ))
    }
  }

  cannot_link = function(sel, num_feats){
    newA = 1 * (1:num_feats) %in% sel
    return(list(A = newA, b = 1))
  }

  # initialize variables
  A = NULL
  b = c()
  rho_vec = c()

  # iterate over provided constraints
  for (t in 1:length(constraint_types)) {				# for each constraint provided
    if (constraint_types[t] == 'max_size') {			# if max-size
      l = max_size(constraint_vars[[t]],
                   num_elements)
    }
    else if (constraint_types[t] == 'must_link'){		# if must-link
      l = must_link(constraint_vars[[t]],
                    num_elements)
    }
    else if (constraint_types[t] == 'cannot_link'){		# if cannot-link
      l = cannot_link(constraint_vars[[t]],
                      num_elements)
    }
    A = rbind(A, l$A)									# add new constraint to A
    b = c(b, l$b)										# add new constraint to b
    rho_vec = c(rho_vec, rep(rho[t], length(l$b)))
  }

  # build block_matrix, if block_list is provided
  if(is.null(block_matrix) & !is.null(block_list)){
    block_matrix = matrix(0, nrow = length(block_list), ncol = max(unlist(block_list)))
    for(i in 1:length(block_list)){
      block_matrix[i,block_list[[i]]] <- 1
    }
  }

  # check consistency of block matrix
  if(!is.null(block_matrix)){
    if(nrow(block_matrix) != num_elements){
      stop('Error: number of elements must match number of blocks, if block constraints are provided')
    }
    if(any(colSums(block_matrix > 0) > 1)){
      stop('Error: more than one block assigned to a single feature')
    }
  }

  return(build.UBayconstraint(
    A = A,
    b = b,
    rho = rho_vec,
    block_matrix = block_matrix))

}

#' Build decorrelation constraints
#' @description Build a cannot link constraint between highly correlated features. The user defines the correlation threshold.
#' @param data the dataset in the `UBaymodel` object
#' @param level the threshold correlation-level
#' @param method the method used to compute correlation; must be one of `pearson`, `spearman` or `kendall`
#' @return a list containing a matrix `A` and a vector `b` representing the inequality system `Ax<=b`, a vector `rho` and a block matrix
#' @importFrom utils combn
#' @export

buildDecorrConstraints = function(data, level = 0.5, method = 'spearman'){

  corr_matrix = cor(data, method = method)
  corr_matrix <- abs(corr_matrix)
  num_features = ncol(corr_matrix)

  A_corr <- matrix(0, nrow = choose(num_features, 2), ncol = num_features)
  A_corr[cbind(rep(1:choose(num_features,2), each = 2), as.vector(combn(num_features,2)))] <- 1
  b_corr <- rep(1, choose(num_features, 2))
  rho_corr <- corr_matrix[lower.tri(corr_matrix)]

  #restrict to positive rho
  pos_corr <- rho_corr > level
  A_corr <- A_corr[pos_corr,]

  if(is.vector(A_corr)){A_corr = t(A_corr)}

  b_corr <- b_corr[pos_corr]
  rho_corr <- rho_corr[pos_corr]
  rho_corr <- rho_corr / (1-rho_corr)# logit function # TODO: transform for level != 0.5!

  return(build.UBayconstraint(
    A = A_corr,
    b = b_corr,
    rho = rho_corr,
    block_matrix = diag(nrow = ncol(data))))

}

#' Build a customized constraint for UBayFS
#' @description Builds a constraint using a left side `A`, a right side `b`, a relaxation parameter `rho`, and a block matrix `block_matrix`.
#' @param A matrix containing the left side of the linear inequality system
#' @param b vector containing the right side of the linear inequality system
#' @param rho vector containing the relaxation parameters for each constraint
#' @param block_matrix a matrix indicating the membership of features in feature blocks
#' @return a `UBayconstraint` object
#' @export

build.UBayconstraint <- function(A, b, rho, block_matrix = NULL){

  if(is.null(block_matrix)){
    block_matrix <- diag(nrow = ncol(A))
  }

  const <- list(A = A,
                b = b,
                rho = rho,
                block_matrix = block_matrix)

  class(const) <- 'UBayconstraint'

  if(is(const, 'UBayconstraint')){
    return(const)
  }
  else{
    stop('Could not produce constraint - check specifications')
  }
}


#' Checks whether a list object implements proper UBayFS user constraints
#' @param x a `UBayconstraint` object
#' @return boolean value
#' @export
is.UBayconstraint <- function(x){

  if(is.null(x)){
    return(TRUE)
  }
  else if(is(x, 'UBayconstraint')){
    return(FALSE)
  }
  else if(!is.list(x)){
    return(FALSE)
  }

  A = x$A
  b = x$b
  rho = x$rho
  block_matrix = x$block_matrix

  if(is.null(A) | is.null(b) | is.null(rho) | is.null(block_matrix)){
    return(FALSE)
  }
  else if(any(rho <= 0)){
    return(FALSE)
  }
  else if(nrow(A) != length(b) | length(b) != length(rho)){
    return(FALSE)
  }
  else if(ncol(A) != ncol(block_matrix) | nrow(block_matrix) != length(b)){
    return(FALSE)
  }

  return(TRUE)
}




#' Set constraints in UBaymodel object
#' @description Set the constraints in a `UBaymodel` object.
#' @param model a `UBaymodel` object created using \link{build.UBaymodel}
#' @param constraints a `UBayconstraint` object created using \link{build.UBayconstraint}
#' @param append if `TRUE`, constraints are appended to the existing constraint system
#' @return a `UBaymodel` object with updated constraint parameters
#' @seealso build.UBaymodel
#' @importFrom methods is
#' @export

setConstraints = function(model, constraints, append = FALSE){

  if(!is(model, 'UBaymodel')){
    stop('Error: wrong class of model')
  }

  if(!is(constraints, 'UBayconstraint') && !is.null(constraints)){
    stop('Error: inconsistent constraints provided!')
  }
  if(!is.null(constraints)){
    # set block matrix for ordinary constraints
    if(ncol(model$data) != ncol(constraints$block_matrix)){
      stop('Error: inconsistent constraints provided')
    }
    if(nrow(constraints$block_matrix) != ncol(constraints$A)){
      stop('Error: inconsistent constraints provided')
    }

    # check whether a constraint with same block_matrix exists
    existing_constraint = 0
    if(length(model$constraint.params) > 0){
      for (i in 1:length(model$constraint.params)) {
        if(identical(model$constraint.params[[i]]$block_matrix, constraints$block_matrix)){
          existing_constraint = i
        }
      }
    }

    # add new constraint or append to old one with same block_matrix
    if(existing_constraint > 0 && append == TRUE){
    const <- model$constraint.params[[existing_constraint]]
      constraints <- list(A = rbind(const$A, constraints$A),
                          b = c(const$b, constraints$b),
                          rho = c(const$rho, constraints$rho),
                          block_matrix = constraints$block_matrix)
      model$constraint.params[[existing_constraint]] <- constraints
    }
    else{
      model$constraint.params = append(model$constraint.params, list(constraints))
      if(existing_constraint > 0){
        model$constraint.params[[existing_constraint]] <- NULL
      }
    }
  }

  return(model)
}

