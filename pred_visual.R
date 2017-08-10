library(ggplot2)
library(ggExtra)
library(cowplot)

dens <- function(fields, cls) {
  indices <- lapply(unique(cls)[-6], function(cl) {
                      sample(which(cls == cl), 1)
             })

  prep_img <- lapply(fields, function(f) {
                       im <- clip(f$image, c("20%", "10%", "19%", "10%"))
                       bin <- !binary(im)
              })

  curve_length <- function(curve) {
    sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
  }


  # plot list
  pl <- lapply(indices, function(ind) {
             bin <- prep_img[[ind]]
             if (all(bin == FALSE)) {
               xy <- expand.grid(x = c(-10:0, 10:20), y = 1:10)
             } else {
               xy <- scale(where(bin))
             }
             xy[,2] <- max(xy[,2]) - xy[,2]
             xy <- as.data.frame(xy)

             xd <- density(xy[,1])
             yd <- density(xy[,2])

             xl <- round(curve_length(xd), 3)
             yl <- round(curve_length(yd), 3)

             ggMarginal(ggplot(xy,aes(x,y)) + geom_point() +
                        xlab(paste("Arc length:",xl)) +
                        ylab(paste("Arc length:",yl)))
        })

  # label list
  labs <- unique(cls)[-6]

  # plot all plots
  plot_grid(plotlist=pl, ncol = 3, labels = labs)
}

