# Plots of x-coords and y-coords of all images 
# colored by NDAI, SD, and CORR respectively
featurePlot <- function(image, feature, feature_name, image_num, include_title, include_legend, plot){
  p1 <- ggplot(image)
  p1 <- p1 + geom_point(aes(x = x_coord, y = y_coord, color = feature), size=0.0)
  p1 <- p1 + scale_color_gradientn(colors = brewer.pal(n = 5, name = "BrBG"))
  p1 <- p1 + labs(colour = feature_name, xlab = "x-coordinate", ylab = "y-coordinate")
  p1 <- p1 + theme_classic()
  p <- p + theme(axis.title = element_text(size = 16), axis.text=element_text(size=14))
  p <- p + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  if (include_title == TRUE) {
    p1 <- p1 + labs(title = paste("Image", image_num))
  }
  if (include_legend == FALSE) {
    p1 <- p1 + theme(legend.position = "none")
  }
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# Kernel density plots of radiance values 
# grouped by classes (cloud, unlabeled, not cloud)
radiancePlot2 <- function (image, feature, label, plot){
  p1 <- ggplot(image)
  p1 <- p1 + geom_density(aes(x = feature,
                          group = factor(label), fill = factor(label)), 
                          alpha = 0.5)
  p1 <- p1 + scale_fill_discrete(name = "Expert label", label = c("Not cloud", "No ID", "Cloud"))
  p1 <- p1 + ylab("Density")
  p1 <- p1 + theme_classic()
  
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plots estimated pdfs for feature by each label 
featureDensity <- function (image, feature, plot) {
  p1 <- ggplot(image %>% mutate(label = ifelse(label == -1,
                                               "Not Cloud",
                                               ifelse(label == 1,
                                                      "Cloud", "Unlabeled"))),
               aes(fill = label, colour = label)) +
    geom_density(aes_string(x = feature), alpha = 0.7) + 
    scale_fill_manual(values = c("dodgerblue", "seagreen2", "burlywood")) +
    xlab(feature) + ylab("Density") +
    theme_bw() + theme(text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5))
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

# plots histogram of each feature in an image
featureHistogram <- function (image, bin_size, plot) {
  p1 <- ggplot(gather(image), aes(value)) + 
    geom_histogram(bins = bin_size, fill = "deepskyblue4") + 
    facet_wrap(~key, scales = 'free_x') +
    xlab("Value") + ylab("Count") +
    theme(text = element_text(size = 12),
          axis.title = element_text(size = 18),
          plot.title = element_text(hjust = 0.5))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# plots the image of expert classification - 1, 0, or -1
classifyPlot <- function (image, plot) {
  p1 <- ggplot(image) +
    geom_point(aes(x = x_coord, y = y_coord,
                   color = factor(label)), alpha = 1.0) +
    scale_colour_manual(name = "Expert Label",
                        values = c("burlywood", "black", "dodgerblue"),
                        labels = c("Not Cloud", "Unlabeled", "Cloud"),
                        guide = guide_legend(override.aes = 
                                               list(size = 9))) +
    theme_bw() + theme(text = element_text(size = 18),
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

# similar to above - compute correlation between two features and scatter plot
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
    theme(text = element_text(size = 12),
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
    theme(text = element_text(size = 12),
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
    xlab("Label") + ylab(feature) + theme_bw() +
    scale_fill_manual(values = c("dodgerblue", "seagreen2", "burlywood")) +
    theme(axis.title = element_text(size = 18), axis.text=element_text(size = 12),
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
    mutate(label = ifelse(label == -1, "Not Cloud", "Cloud")) %>%
    ggplot(aes(fill = ..level..)) +
    stat_density_2d(aes_string(x = feature1, y = feature2, color = "label"),
                    alpha = 0.7, geom = "polygon") +
    scale_fill_distiller(palette = "Blues", direction = 1) + 
    xlab(feature1) + ylab(feature2) + theme_bw() +
    theme(text = element_text(size = 12),
          legend.text = element_text(size = 6))
  if (plot == TRUE) {
    print(p1)
  }
  return (p1)
}

# # QQ plot of each feature
# make_qq_plot <- function(train_set, variable) {
#   # correct NDSI column name
#   if (variable == "NDSI") {
#     title <- "NDAI"
#   } else {
#     title <- variable
#   }
#   
#   # train_set[, variable] <- as.vector(scale(train_r[, variable]))
#   
#   plot <- scale(train_set) %>%
#     sample_frac(.2) %>% 
#     ggplot() + 
#     geom_qq(aes_string(sample = variable, color = "label")) + 
#     geom_abline(intercept = 0, slope = 1, lwd=2) +
#     scale_color_manual(name = "Label", breaks = c(-1, 1),
#                        labels = c("Not cloud", "Cloud"),
#                        values=c("brown", "dodgerblue")) +
#     ggtitle(title) + 
#     theme_classic()
#   
#   return(plot)
# }

# QQ plot of each feature
make_qq_plot <- function(train_set, variable) {
  # correct NDSI column name
  if (variable == "NDSI") {
    title <- "NDAI"
  } else {
    title <- variable
  }
  
  cloud_train_set <- subset(train_set, label == 1)
  cloud_feature <- cloud_train_set[variable]
  scaled_cloud_feature <- data.frame(scale(cloud_feature, center = TRUE, scale = TRUE))
  final_cloud_feature <- sample_frac(scaled_cloud_feature, 0.2)
  
  non_cloud_train_set <- subset(train_set, label == -1)
  non_cloud_feature <- non_cloud_train_set[variable]
  scaled_non_cloud_feature <- data.frame(scale(non_cloud_feature, center = TRUE, scale = TRUE))
  final_non_cloud_feature <- sample_frac(scaled_non_cloud_feature, 0.2)
  
  plot <- ggplot() +
    stat_qq(aes(sample = final_cloud_feature[,variable], color = "Cloud")) +
    stat_qq(aes(sample = final_non_cloud_feature[,variable], color = "Non cloud")) + 
    geom_abline(intercept = 0, slope = 1, lwd = 2) +
    scale_colour_manual("", values = c("Cloud"="dodgerblue", "Non cloud"="brown")) +
    ggtitle(variable) +
    theme_classic() 
  
  return(plot)
}

# Returns a color between col1 and col2.
# divides the interval btw two color by n colors, returns kth color
# k = 1 returns col1
# k = n returns col2
colorBtw = function(col1, col2, n, k) {
  return(colorRampPalette(c(col1, col2))(n)[k])
}
# This function plots the image with predicted value.
classifyPostPlot = function (image, plot) {
  
  # For original expert labeled data, the points are in vivid color
  # For unlabeled data, predicted labels were used, and indicated in faded color.
  image$col = 2 * image$label
  image$col[which(image$label == 0)] = image$pred[which(image$label == 0)]
  alpha = 1 - 0.5 *(image$label == 0)
  p1 = ggplot(image) +
    geom_point(aes(x = x_coord, y = y_coord,
                   color = factor(col), alpha = alpha) ) +
    scale_colour_manual(name = "",
                        values = c("burlywood", colorBtw("burlywood", "white", 5, 2), 
                                   colorBtw("dodgerblue", "white", 5, 2), "dodgerblue"),   
                        labels = c("Not Cloud", "Unlabeled Not Cloud", "Unlabeled Cloud", "Cloud"),
                        #guide = guide_legend(override.aes = list(size = 9))) +
                        guide = 'none') + 
    scale_alpha(guide = 'none') +
    theme_bw() + theme(text = element_text(size = 12),
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
