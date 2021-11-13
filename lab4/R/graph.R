# plots estimated pdfs for feature by each label 
featureDensity <- function (image, feature, plot) {
  p1 <- ggplot(image %>% mutate(label = ifelse(label == -1,
                                               "Not Cloud",
                                               ifelse(label == 1,
                                                      "Cloud", "Unlabeled"))),
               aes(fill = label, colour = label)) +
    geom_density(aes_string(x = feature), alpha = 0.1) + 
    xlab(feature) + ylab("Density") +
    theme(text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plots histogram of each feature in an image
featureHistogram <- function (image, bin_size, plot) {
  p1 <- ggplot(gather(image), aes(value)) + 
    geom_histogram(bins = bin_size, fill = "deepskyblue4") + 
    facet_wrap(~key, scales = 'free_x') +
    xlab("Value") + ylab("Count") +
    theme(text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plots the black, grey and white image of expert classification
classifyPlot <- function (image, plot) {
  p1 <- ggplot(image) +
    geom_point(aes(x = x_coord, y = y_coord,
                   color = factor(label)), alpha = 0.1) +
    scale_colour_manual(name = "Expert Label",
                        values = c("grey", "black", "white"),
                        labels = c("Not Cloud", "Unlabeled", "Cloud"),
                        guide = guide_legend(override.aes = 
                                               list(alpha = 0.8))) +
    theme_bw() + theme(text = element_text(size = 11),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position = "bottom",
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       plot.background = element_blank())
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
} 

# basic correlation plot of all the features
corrMap <- function (image, plot) {
  corr_figs <- cor(image)
  p1 <- corrplot(corr_figs, type = 'lower', order = 'hclust', tl.col = 'black', 
                 cl.ratio = 0.2, tl.srt = 30, tl.cex = 0.5)
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# computes correlation between features and a scatter plot for both cloud 
# and not cloud against the features
radiancePlot <- function (image, feature1, feature2, plot) {
  # feature1, feature2 - string arguments - i.e. "DF", "BF"
  p1 <- image %>% filter(label != 0) %>%
    mutate(label = ifelse(label == -1, "Not Cloud", "Cloud")) %>%
    # scatterplot w/ line and rho
    ggplot(aes_string(x = feature1, y = feature2)) +
    geom_point(aes(color = factor(label)), alpha = 0.7) +
    facet_wrap(~label, ncol = 2) +
    stat_cor(aes(label = ..r.label..), 
             method = 'kendall', 
             cor.coef.name = "rho", 
             r.accuracy = 0.01,
             size = 3) +
    geom_abline(slope = 1, intercept = 0, color = 'black',
                linetype = 'dashed', size = 0.7) +
    scale_colour_manual(values = c("deepskyblue", "palegreen2")) +
    theme(text = element_text(size = 7),
          legend.position = "none")
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plotting 2 radiances versus one of the three metrics 
# you have to edit the x, y and z coordinates FYI
# very nice 3d interactive visual of three features for cloud vs. not cloud
threePlot <- function (image) {
  p1 <- plot_ly(image %>%
                  filter(label != 0) %>%
                  mutate(label = ifelse(label == -1, "Not Cloud", "Cloud")),
                x = ~DF, y = ~CF, z = ~CORR, color = ~factor(label)) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = "DF"),
                        yaxis = list(title = "CF"),
                        zaxis = list(title = "CORR")))
  return (p1)
}

# histogram plot of feature for both cloud and not cloud labels
histogramCompare <- function (image, feature, plot) {
  p1 <- image %>% filter(label != 0) %>%
    mutate(label = ifelse(label == -1, "Not Cloud", "Cloud")) %>%
    ggplot(aes_string(x = feature,)) +
    geom_histogram(fill = "slateblue2") +
    facet_wrap(~label, ncol = 2) + ylab("Count") +
    theme(text = element_text(size = 7),
          legend.position = "none")
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# boxplot of feature for cloud, not cloud and unlabeled
boxplotCompare <- function (image, feature, plot) {
  p1 <- image %>%
    mutate(label = ifelse(label == -1,
                          "Not Cloud",
                          ifelse(label == 1,
                                 "Cloud", "Unlabeled"))) %>%
    ggplot(aes_string(y = feature)) +
    geom_boxplot(aes(x = label, fill = label)) +
    xlab("Label") + ylab(feature) + 
    theme(text = element_text(size = 7),
          legend.position = "none")
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# estimates joint pdf of feature1 and feature2 for both cloud and non cloud labels
contourCompare <- function (image, feature1, feature2, plot) {
  p1 <- image %>%
    filter(label != 0) %>%
    mutate(label = ifelse(label == -1, "Not Cloud", "Cloud")) %>% ggplot(aes(fill = ..level..)) +
    stat_density_2d(aes_string(x = feature1, y = feature2, color = "label"), alpha = 0.7, geom = "polygon") +
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    xlab(feature1) + ylab(feature2) +
    theme(text = element_text(size = 7))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plotting accuracy of KNN
knnAccuracyPlot <- function (data, plot) {
  p1 <- ggplot(data, aes(x = k, y = accuracy)) +
    geom_point() + geom_line(size = 1.2) + 
    geom_vline(xintercept = c(31), linetype = "dotted") +
    ylab("Accuracy") +
    geom_label(data = . %>% filter(accuracy == max(accuracy)),
               aes(label = paste("31,", sprintf('%0.2f', accuracy))),
               hjust = -0.2, size = 3) + 
    theme(text = element_text(size = 9))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# return plot of ROC for multiple trained objects
rocPlot <- function (roc_df, plot) {
  # roc_df: df of roc with type column for each trained object
  p1 <- ggplot(roc_df) +
    geom_line(size = 1.4, aes(x = false_pos, y = true_pos, color = type)) +
    geom_abline(intercept = 0, slope = 1, linetype = 3, colour = "red") +
    xlab("Average False Positive Rate") + ylab("Average True Positive Rate") +
    theme(text = element_text(size = 7)) +  
    guides(color = guide_legend(title = "KNN Model")) +
    scale_colour_manual(values = c("deepskyblue3", "seagreen3"))
  if (plot == TRUE) {
    print(p1)
  }
  return(p1)
}

# plotting convex hull of classifications
boundaryPlot <- function (test, prediction, feature1, feature2, plot) {
  # use convex hull to determine boundary points of clusters
  boundary_df <- data.frame(x = test[, feature1], 
                            y = test[, feature2], 
                            Prediction = prediction)
  findHull <- function(df) {
    df[chull(df$x, df$y),]
  }
  boundary <- plyr::ddply(boundary_df, .variables = "Prediction", .fun = findHull)
  p1 <- ggplot() + 
    geom_point(data = boundary_df, size = 2,
               aes(x = x, y = y, color = Prediction, fill = Prediction)) +
    geom_polygon(data = boundary, alpha = 0.4, aes(x = x, y = y, fill = Prediction, color = Prediction)) +
    xlab(feature1) + ylab(feature2) + 
    scale_fill_manual(name = "Prediction",
                      label = c("Not Cloud", "Cloud"),
                      values = c("deepskyblue", "palegreen2")) + 
    scale_color_manual(name = "Prediction",
                       label = c("Not Cloud", "Cloud"),
                       values = c("blueviolet", "springgreen4")) +
    theme(text = element_text(size = 7))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}