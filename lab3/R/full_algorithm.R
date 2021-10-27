# run k-means in parallel

# load necessary pacakges
library(foreach)
library(doParallel)
library(Rcpp)
library(tidyverse)

# load cpp file for computing similarity
sourceCpp('R/similarity.cpp')

# helper function to run kmeans on sample for k clusters
kMeans <- function(sample, k){
  X <- sample %>%
    select(-ID)
  
  kmeans <- kmeans(X, centers = k)
  
  # join ID column and cluster assignments
  cluster <- cbind(sample$ID, kmeans$cluster) %>%
    as.data.frame()
  
  colnames(cluster) <- c("ID", "cluster")
  return(cluster)
}

# Compute the correlation between the cluster assignments of two subsamples
getKmeansSimilarity <- function(data, m, k){
  # Args:
  #   data : dataframe, contains ID and the question columns
  #   m : double, sample fraction 
  #   k : integer, cluster number
  # Returns:
  #   corr : double, correlation value
  
  # make two subsamples
  sample_1 <- data %>%
    sample_frac(m) %>%
    arrange(ID)
  sample_2 <- data %>%
    sample_frac(m) %>%
    arrange(ID)
  
  # run kmeans on the two subsamples
  x <- kMeans(sample_1, k)
  y <- kMeans(sample_2, k)
  
  # Filter rows of x so that it only contains individuals given by ids
  filterRows <- function(x, ids){
    # keep rows whose ID is in ids
    x <- x %>%
      filter(ID %in% ids)
    
    # return the cluster assignment as an interger vector
    return(as.numeric(x$cluster))
  }
  
  # find common IDs between x and y
  good_ids <- intersect(x$ID, y$ID)
  # keep only the individuals who are in both x and y
  x_unique <- filterRows(x, good_ids)
  y_unique <- filterRows(y, good_ids)
  
  return(similarityRcpp(x_unique, y_unique))
}

fullKmeansTest <- function(data, m, k, N){
  # Compute the correlation for kmeans between two subsamples. Repeat N times.
  # Args:
  #   data : dataframe, contains ID and the question columns
  #   m : double, sample fraction 
  #   k : integer, cluster number
  #   N : integer, number of experiments
  # Returns:
  #   corr : N x 1 dataframe, contains N correlation values. The column name is set to "k".
  
  # compute the correlation N times
  result <- lapply(1:N, function(x) getKmeansSimilarity(data, m, k)) %>%
    # unpack the list
    unlist() %>%
    # convert to a dataframe
    as.data.frame()
  
  # rename the column: k1, k2, ...
  colnames(result) <- paste0('k', k)
  return(result)
}


# use 9 cores (i.e. one for each k)
registerDoParallel(9) 

# parameters for stability testing
N <- 100 # repeat 100 times for each k
m <- .5 # sample fraction
k_max <- 10 # maximum number of k to try

# load data
load('data/lingBinary.RData')
# select only the ID and the question columns
data <- select(lingBinary, -CITY, -STATE, -ZIP, -lat, -long)

# run kmeans stability test in parallel for k = 2, ..., k_max
result <- foreach(k = 2:k_max) %dopar% {
  fullKmeansTest(data, m, k, N)
}

# combine results into a single dataframe. Each column corresponds to a particualr k.
result_df <- result %>%
  # combine columns
  bind_cols() %>%
  # save only 3 decimal points
  format(digits = 3)

# save results
write.table(result_df, "results.csv")
