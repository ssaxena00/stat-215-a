
cleanLingData <- function(ling_data_df){
  # Arguments:
  #   ling_data_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a data.frame with renamed column names
  
  # filter data for which long or lat are NA
  ling_data_df <- ling_data_df %>% 
    # convert column names to lower case
    rename_all(tolower) %>%
    # filter hawaii and alaska
    filter(long > -150 & lat < 50 & lat > 24) %>%
    # filter any rows with missing latitude or longtitude; filter out Hawaii and Alaska
    filter(state != "HI" & state != "AK")  %>%
    # replace 0 with NA
    na_if(0) %>%
    # remove any rows with NA
    na.omit()
  
  return(ling_data_df)
}

cleanLingLocation <- function(ling_location_df){
  # Arguments:
  #   ling_data_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a data.frame with renamed column names
  
  # filter data for which long or lat are NA
  ling_location_df <- ling_location_df %>% 
    # filter non lower 48 states
    filter(Longitude > -150 & Latitude < 50) %>%
    # rename columns
    rename(n = Number.of.people.in.cell, lat = Latitude, long = Longitude) %>% 
    # convert column names to lower case
    rename_all(tolower)
  
  return(ling_location_df)
}

# utility functions below

blank_theme <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 

plotMap <- function(responses, state_df, my_title){
  # Plot responses on the map of US
  # Args:
  #   responses : data frame, contains log, lat, response to question
  #   state_df: data frame, contains state log and lat data
  #   my_title: string, title of this plot
  # Returns:
  #   plot : ggplot object
  
  plot <- ggplot() + 
    geom_point(data = responses, aes(x = long, y = lat, color = ans), size = .3, alpha = .5) + 
    geom_polygon(data = state_df, aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
    scale_color_discrete(name = "Answer") + 
    labs(subtitle = my_title) + 
    theme_void()
  
  return(plot)
}

getDataForQuestion <- function(ling_data, question_num, which_answers){
  # Make a dataframe that contains long, lat, and the responses to the questions given
  # Args:
  #   question_num : string, question number (e.g. "065")
  #   which_answers: vector, contains which answer categories to select 
  # (e.g. c(1,2) means only look at those who answered either category 1 or 2)
  # Returns:
  #   responses: dataframe, containing the data for the given question, plus long and lat
  
  # filter data for a given question number
  responses <- ling_data %>%
    # select relevant columns
    select(id, lat, long, response = paste0("q", question_num)) %>%
    # just look at observations who chose answers specified in which_answers
    filter(response %in% which_answers & !is.na(response)) %>%
    # convert answers to chracters
    mutate(response = as.character(response)) %>%
    #downsample
    sample_frac(1)
  
  # answer choices
  choices <- all.ans[[as.numeric(question_num)]] %>%
    # append a column to join responses by
    mutate(response = rownames(.))
  
  # join data
  responses <- responses %>%
    # join
    inner_join(choices, by = "response") %>%
    # update factors for answers since only a subset was chosen
    droplevels()

  return(responses)  
}



convertToBinary <- function(col_name, data){
  # Convert catgorical data to binary
  # Args:
  #   col_name : string, question number (e.g. "q065")
  #   data: dataframe, contains binary data
  # Returns:
  #   binary : dataframe, binary encoding of the categorical data
  
  binary <- data %>%
    # select id column and question column
    select(id, col_name) %>%
    # append a column of ones
    mutate(present = 1) %>%
    # spread the data to convert the categorical encoding to binary
    spread(col_name, present, fill = 0) %>%
    # remove the id column
    select(-id)
  
  colnames(binary) <- sapply(1:ncol(binary), function(x) paste0(col_name, x))
  return(binary)
}

rotateData <- function(X){
  # Run PCA on the given data, X, and return the projection of the data onto the PC space
  # Args:
  #   X : dataframe, contains the binary encoding of the data, along with long and lat
  # Returns:
  #   Y : dataframe, rotated data
  
  # run PCA
  pca <- X %>%
    # remove the location columns
    select(-long, -lat) %>%
    # scale data
    scale(center = FALSE, scale = FALSE) %>%
    # compute the covariance
    cov() %>%
    # find the eigenvalues and eigenvectors
    eigen()
  
  # project data onto PC space
  Y <- X %>%
    # remove the location columns
    select(-long, -lat) %>%
    # scale the data
    scale(center = FALSE, scale = FALSE) %>%
    # project onto the PC space
    as.matrix() %*% pca$vectors %>%
    # convert from matrix to dataframe
    data.frame() %>%
    # add back the location columns
    mutate(long = X$long, lat = X$lat)
  
  return(Y)
}

plotKmeans <- function(kmeans, data, alpha, sample_rate){
  # Plot kmeans clusters on the map of US
  # Args:
  #   kmeans : output of the kmeans function
  #   data: dataframe, contains long and lat
  # Returns:
  #   ggplot object
  
  data <- data %>%
    mutate(cluster = kmeans$cluster) %>%
    sample_frac(sample_rate)
  
  plot <- ggplot(data) + 
    geom_point(aes(x = long, y = lat, color = as.factor(cluster)), alpha = alpha) + 
    geom_polygon(data = state_df, aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
    labs(color = "Cluster") + 
    theme_void()
  
  return (plot)
}
