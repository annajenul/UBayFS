#' @export
# help function to build constraint inequation system from user inputs

build_constraints = function(constraint_types, constraint_vars, num_features, rho = 1){

  # help functions for constraint types
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





