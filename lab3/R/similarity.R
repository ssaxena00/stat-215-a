
similarity <- function(x1, x2) {
  # Computes the similarity measure of 2 vectors (from Fowlkes and Mallows).
  # Dot product computes # of pairs of points clustered together
  # Then it is normalized into a correlation (cosine) similarity
  # Args:
  #   x1 : integer vector, cluster assignment for sample 1
  #   x2 : integer vector, cluster assignment for sample 2
  # Returns:
  #   sim: double, correlation similarity 
  
  n1 = length(x1)
  n2 = length(x2)
  
  # error out if lengths of vectors are not same
  if (n1 != n2)
    return(NULL)
  
  # compute <L1, L1>, <L2, L2>, <L1, L2> described in Ben-Hur
  l1_l1 = 0
  l2_l2 = 0
  l1_l2 = 0
  
  # iterate through each pair (i, j)
  for (i in 1:n1) {
    for (j in i+1:n1) {
      # break loop if index j is out of bounds
      if(j > n) break()
      
      # compute C_ij for x1 and for x2
      c1_ij <- 0 
      if (x1[i] == x1[j]) {
        c1_ij <- 1
      }
      c2_ij <- 0
      if (x2[i] == x2[j]) {
        c2_ij <- 1
      }
      
      # update dot products
      l1_l1 <- l1_l1 + c1_ij * c1_ij
      l2_l2 <- l2_l2 + c2_ij * c2_ij
      l1_l2 <- l1_l2 + c1_ij * c2_ij 
    }    
  }
  
  # return correlation sim
  sim <- l1_l2 / (sqrt(l1_l1) * sqrt(l2_l2))
  return(sim)
}
