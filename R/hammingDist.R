# function to calculate Hamming distance of two binary vectors
hammingDist = function(vec1, vec2){
  return(
    1 - (sum(vec1 != vec2) / length(vec1))
  )

}

