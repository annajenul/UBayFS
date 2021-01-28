#' Build constraint system
#' @description builds a inequation system from constraints provided by the user
#' @details The function transforms user information about relations between features (must-link or cannot-link constraints) and maximum feature set size (max-size) into a linear inequation system. In addition, the relaxation paramter rho can be specified to achieve soft constraints.
#' @param constraint_types a vector of strings denoting the type of constraint to be added; options: "max_size", "must_link", "cannot_link"
#' @param constraint_vars a list of parameters defining the constraints; in case of max-size constraints, the list element must contain an integer denoting the maximum size of the feature set, in case of max-link or cannot link, the list element must be a vector of feature indices to be linked
#' @param num_features the total number of features in the dataset
#' @param rho a positive parameter denoting the level of relaxation; Inf denotes no relaxation
#' @return a list constaining a matrix A and a vector b representing the inequality system Ax<=b, and a vector rho
#' @examples
#' # given a dataset with 10 features, we create a max-size constraint limiting
#' # the set to 5 features and a cannot-link constraint between features 1 and 2
#' build_constraints(constraint_types = c("max_size","cannot_link"),
#' constraint_vars = list("5", c(1,2)),
#' num_features = 10,
#' rho = 1)
#' @export

build_constraints = function(constraint_types, constraint_vars, num_features, rho = 1){

  # check input
  if(!all(constraint_types %in% c("max_size", "must_link", "cannot_link"))){
    stop("Error: wrong constraint type provided")
  }
  if(length(constraint_types) != length(constraint_vars)){
    stop("Error: constraint_vars must have same length as constraint_types")
  }
  if(num_features <= 0 | num_features %%1 != 0){
    stop("Error: num_features must be a positive integer")
  }
  if(rho <= 0 | length(rho) > 1){
    stop("Error: rho must be a single positive value")
  }

  # define constraints
  max_size = function(smax, num_feats){
    return(list(A = matrix(1, nrow = 1,
                           ncol = num_feats),
                b = smax))
  }

  must_link = function(sel, num_feats){
    if(length(sel) > 1){
      pairs = expand.grid(sel, sel) 					# all pairs
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

  # iterate over provided constraints
  for (t in 1:length(constraint_types)) {				# for each constraint provided
    if (constraint_types[t] == "max_size") {			# if max-size
      l = max_size(constraint_vars[[t]],
                   num_features)
    }
    else if (constraint_types[t] == "must_link"){		# if must-link
      l = must_link(constraint_vars[[t]],
                    num_features)
    }
    else if (constraint_types[t] == "cannot_link"){		# if cannot-link
      l = cannot_link(constraint_vars[[t]],
                      num_features)
    }
    A = rbind(A, l$A)									# add new constraint to A
    b = c(b, l$b)										# add new constraint to b
  }
  return(
    list(A = A,
         b = b,
         rho = rep(rho, length(b)))
  )
}





