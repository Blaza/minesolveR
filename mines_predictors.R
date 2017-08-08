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


# The predictors part

#' Calculte the length of a curve
#'
#' @param curve - a list with $x and $y elements representing the x and y
#'                coordinates of all points of the curve ('discrete' curve)
#' @return The arc length of the curve
curve_length <- function(curve) {
  sum(sqrt(diff(curve$x)^2 + diff(curve$y)^2))
}

#' Calculate the arc length of the x-coordinate density curve
#'
#' @param im - the image representing the field to classify
#' @return The arc length of the x-coordinate density of the number on the image
x_arc_length <- function(im) {
  # first we'll clip the image to remove the bordes and shades
  im <- clip(im, c("20%", "10%", "19%", "10%"))
  # then we binarize the image (it will probably already be binary but that
  # doesn't change anything) and take the negative so the numbers are white
  # instead of black on white background
  bin <- !binary(im)

  # In the case that almost all pixels are black, we'll make a dummy image of
  # a shape which cannot be mistaken for a number (could've just said return(0)
  # but this was my first idea and it is a bit more interesting, althoug a big
  # overkill)
  if (mean(bin == FALSE) > 0.95) {
    xy <- expand.grid(x = c(-2:0, 2:4), y = 1:10)
  } else {
    # we scale the xy coordinates to have all images on same scale
    xy <- scale(where(bin))
  }

  xy <- as.data.frame(xy)

  # we get the density of the x coordinates and return the arc length
  xd <- density(xy[ , 1])
  curve_length(xd)
}


#' Calculate the arc length of the y-coordinate density curve
#'
#' @param im - the image representing the field to classify
#' @return The arc length of the y-coordinate density of the number on the image
y_arc_length <- function(im) {
  # first we'll clip the image to remove the bordes and shades
  im <- clip(im, c("20%", "10%", "19%", "10%"))
  # then we binarize the image (it will probably already be binary but that
  # doesn't change anything) and take the negative so the numbers are white
  # instead of black on white background
  bin <- !binary(im)

  # In the case that almost all pixels are black, we'll make a dummy image of
  # a shape which cannot be mistaken for a number (could've just said return(0)
  # but this was my first idea and it is a bit more interesting, althoug a big
  # overkill)
  if (mean(bin == FALSE) > 0.95) {
    xy <- expand.grid(x = c(-2:0, 2:4), y = 1:10)
  } else {
    # we scale the xy coordinates to have all images on same scale
    xy <- scale(where(bin))
  }

  xy <- as.data.frame(xy)

  # we get the density of the y coordinates and return the arc length
  yd <- density(xy[ , 2])
  curve_length(yd)
}

