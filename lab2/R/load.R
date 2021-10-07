
loadLingData <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the `lingData.dat`
  #         Path should be relative to the lab2.Rmd file.
  # Returns:
  #   a data.frame with three columns: numbers, dates and days.
  
  ling_data <- read.table(paste0(path, "lingData.txt"), header = TRUE)
  return(ling_data)
}

loadLingLocation <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the `lingLocation.dat`
  #         Path should be relative to the lab2.Rmd file.
  # Returns:
  #   a data.frame with three columns: numbers, dates and days.
  
  ling_loc <- read.table(paste0(path, "lingLocation.txt"), header = TRUE)
  return(ling_loc)
}
