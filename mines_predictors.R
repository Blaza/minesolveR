library(imager)
library(ssoftveR)

#' Decolor image
#'
#' Main filter, turns the image into 8-valued image which can be easily
#' thresholded to get nice distinction between numbers and background.
#' First we threshold the image to get 3-bit image which has a lot of contrast
#' and then we split the thresholded image into 8 levels
#'
#' @param im - the 3-channel image to decolor
#' @return A cimg image which is a 8 value version of im
decolor <- function(im) {
  # get 3-bit image with thresholding (three channels, all TRUE / FALSE)
  thr <- im %>% threshold
  # the transformation is a basic mapping from 3-bit logical to range 0-7
  # We're giving lowest weight to the blue channel which has a bonus benefit
  # that most closed fields become black after binarizing
  as.cimg(3 * thr[ , , 1, 1] + 2 * thr[ , , 1, 2] + thr[ , , 1, 3])
}


#' Binarize image
#'
#' Decolors the image, than applies one more threshold to split the image
#' into light and dark pixels. Nicely distinguishes numbers in fields and
#' background.
#'
#' @param im - the image to binarize (can be cimg or decolor image)
#' @param thr - the value to represent the threshold (on 0-1 color scale)
#' @return A binary image (pixset)
binary <- function(im, thr = 4/7) {
  # if the image isn't one channeled, decolor it first
  if (dim(im)[4] != 1) {
    im %>% decolor %>% renorm(., 0, 1) %>% threshold(., thr)
  } else {
    im %>% renorm(., 0, 1) %>% threshold(., thr)
  }
}

#' The image preprocessing function
#'
#' This function is used on the board image before calculating predictors
#'
#' @param im - the image to process
#' @return The processed image
process_img <- function(im) im %>% isoblur(., 2.16) %>% binary


#' Get density dominant mode location
#'
#' @param x - the vector for which to get dominant mode of the density
#' @return The location of the dominant mode of the density(x). If multiple
#'         modes are found, the mean location is taken.
dmode <- function(x) {
      den <- density(x, kernel = c("gaussian"))
      mean(den$x[den$y == max(den$y)])
}


# The predictors part

#' Location of the most frequent row
#'
#' @param im - the field image (will be converted to binary and clipped)
#' @return The location of the dominant mode of rowMeans.
row_dominant_mode <- function(im) {
  bin <- binary(im)
  # clip the field image.
  # This ratio has been found to give nice results
  bin <- clip(!bin, c("25%", "15%", "25%", "15%"))

  dmode(rowMeans(bin[,,1,1]))
}


#' Location of the most frequent row
#'
#' @param im - the field image (will be converted to binary and clipped)
#' @return The location of the dominant mode of colMeans.
col_dominant_mode <- function(im) {
  bin <- binary(im)
  # clip the field image.
  # This ratio has been found to give nice results
  bin <- clip(!bin, c("25%", "15%", "25%", "15%"))

  dmode(colMeans(bin[,,1,1]))
}


curve_length <- function(curve) {
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}

x_arc_length <- function(im) {
   im <- clip(im, c("20%", "10%", "19%", "10%"))
   bin <- !binary(im)

   if (all(bin == FALSE)) {
     xy <- expand.grid(x = c(-2:0, 2:4), y = 1:10)
   } else {
     xy <- scale(where(bin))
   }

   xy <- as.data.frame(xy)

   xd <- density(xy[,1])
   curve_length(xd)
}

y_arc_length <- function(im) {
   im <- clip(im, c("20%", "10%", "19%", "10%"))
   bin <- !binary(im)

   if (all(bin == FALSE)) {
     xy <- expand.grid(x = c(-2:0, 2:4), y = 1:10)
   } else {
     xy <- scale(where(bin))
   }

   xy <- as.data.frame(xy)

   yd <- density(xy[,2])
   curve_length(yd)
}
