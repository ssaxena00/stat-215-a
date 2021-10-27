#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double similarityRcpp(NumericVector x1, NumericVector x2) {
  // Computes the similarity measure of 2 vectors (from Fowlkes and Mallows).
  // Dot product computes # of pairs of points clustered together
  // Then it is normalized into a correlation (cosine) similarity
  // Args:
  //   x1 : integer vector, cluster assignment for sample 1
  //   x2 : integer vector, cluster assignment for sample 2
  // Returns:
  //   sim: double, correlation similarity 
  
  int n1 = x1.size();
  int n2 = x2.size();
  
  // error out if lengths of vectors are not same
  if (n1 != n2) {
    Rcout << "Error: the size of x1 and x2 must be the same.";
    return 0;
  }
      
  // compute <L1, L1>, <L2, L2>, <L1, L2> described in Ben-Hur
  double l1_l1 = 0;
  double l2_l2 = 0;
  double l1_l2 = 0;
      
  // iterate through each pair (i, j)
  for (int i = 0; i < n1; i++) {
    for (int j = 0; j < n1; j++) {
      // compute C_ij for x1 and for x2
      double c1_ij = (x1[i] == x1[j]) ? 1 : 0;
      double c2_ij = (x2[i] == x2[j]) ? 1 : 0;
          
      // update dot products
      l1_l1 += c1_ij * c1_ij;
      l2_l2 += c2_ij * c2_ij;
      l1_l2 += c1_ij * c2_ij;
    }    
  }
      
  // return correlation sim
  double sim = l1_l2 / (sqrt(l1_l1) * sqrt(l2_l2));
  return sim;
}
