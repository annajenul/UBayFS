#' @export

generate_constraints = function(constraint_types, constraint_vars, num_features, rho){

  max_size = function(num_feats, smax){

    return(list(A = matrix(1, nrow = 1, ncol = num_feats),
                b = smax))
  }

  must_link = function(sel, num_feats){

    if(length(sel) > 1){
      pairs <- expand.grid(sel, sel) # all pairs
      pairs <- pairs[-which(pairs[,1] == pairs[,2]),] # delete main diagonal

      newA <- t(apply(pairs, 1, function(x){return(((1:num_feats) == x[1]) - ((1:num_feats) == x[2]))}))

      return(list(A = newA,
                  b = rep(0, nrow(newA))))
    }
  }

  cannot_link = function(sel, num_feats){

    if(length(sel) > 1){
      pairs <- expand.grid(sel, sel) # all pairs
      pairs <- pairs[-which(pairs[,1] <= pairs[,2]),] # delete main diagonal & lower triangle

      newA = t(apply(pairs, 1, function(x){return(((1:num_feats) == x[1]) + ((1:num_feats) == x[2]))}))
      return(list(A = newA,
                  b = rep(1, nrow(newA))))
    }
  }

  A = NULL
  b = c()
  for (t in 1:length(constraint_types)) {
    if (constraint_types[t] == "max_size") {
      l = max_size(num_features, constraint_vars[[t]])
    }
    else if (constraint_types[t] == "must_link"){
      l = must_link(constraint_vars[[t]], num_features)
    }
    else if (constraint_types[t] == "cannot_link"){
      l = cannot_link(constraint_vars[[t]], num_features)
    }
    A = rbind(A, l$A)
    b = c(b, l$b)
  }
  return(list(A = A, b = b, rho = rep(rho, length(b))))
}





