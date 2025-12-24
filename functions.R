# Libraries
library(fields)
library(MASS)
library(imager)
library(writexl)
library(progress)
library(tidyverse)

###################################################
######### Functions for processing images #########
###################################################

# Opens an image, converts it to grayscale, and returns an array.
openImage <- function(path){
  img <- load.image(path)
  img <- grayscale(img)
  img_matrix <- as.array(img)[,,1,1]
  return(img_matrix)
}

# Rotation of a square image.
rotateImage <- function(img, ang){
  l <- dim(img)[1]
  im <- as.cimg(img)
  im_rot <- imrotate(im, ang)
  img_rot <- as.array(im_rot)[,,1,1]
  l_rot <- dim(img_rot)[1]
  w_rot <- dim(img_rot)[2]
  l_dif <- l_rot - l
  w_dif <- w_rot - l
  img_rot <- img_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
  return(img_rot)
}

# Rescaling a square image to an lxl matrix
scaleImage <- function(img, l){
  img <- as.cimg(img)
  img <- resize(img, l, l)
  img_matrix <- as.array(img)[,,1,1]
  return(img_matrix)
}


######################
##### Estimators #####
######################

# Angular Variogram Estimator
VarAng <- function(img, sep = 1){
  if(180%%sep != 0){
    stop("sep parameter must be divisor of 180")
  }
  N <- 180/sep
  var_ang <- numeric(N)
  l <- dim(img)[1]
  im <- as.cimg(img)
  ones <- matrix(1, l, l)
  im_ones <- as.cimg(ones)
  for(i in 1:N){
    im_rot <- imrotate(im, sep*i)
    img_rot <- as.array(im_rot)[,,1,1]
    im_ones_rot <- imrotate(im_ones, sep*i)
    ones_rot <- as.array(im_ones_rot)[,,1,1]
    l_rot <- dim(ones_rot)[1]
    w_rot <- dim(ones_rot)[2]
    l_dif <- l_rot - l
    w_dif <- w_rot - l
    img_rot <- img_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    ones_rot <- ones_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    n <- sum(ones_rot == 1)
    var_ang[i] <- sum((img[ones_rot == 1]-img_rot[ones_rot == 1])^2)/n
  }
  return(var_ang)
}

# Angular Cross-Variogram Estimator
VarAngCr <- function(img1, img2, sep = 1){
  if(180%%sep != 0){
    stop("sep parameter must be divisor of 180")
  }
  N <- 180/sep
  var_ang_cr <- numeric(N)
  l <- dim(img1)[1]
  im1 <- as.cimg(img1)
  im2 <- as.cimg(img2)
  ones <- matrix(1, l, l)
  im_ones <- as.cimg(ones)
  for(i in 1:N){
    im_rot1 <- imrotate(im1, sep*i)
    im_rot2 <- imrotate(im2, sep*i)
    img_rot1 <- as.array(im_rot1)[,,1,1]
    img_rot2 <- as.array(im_rot2)[,,1,1]
    im_ones_rot <- imrotate(im_ones, sep*i)
    ones_rot <- as.array(im_ones_rot)[,,1,1]
    l_rot <- dim(ones_rot)[1]
    w_rot <- dim(ones_rot)[2]
    l_dif <- l_rot - l
    w_dif <- w_rot - l
    img_rot1 <- img_rot1[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    img_rot2 <- img_rot2[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    ones_rot <- ones_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    n <- sum(ones_rot == 1)
    var_ang_cr[i] <- sum((img1[ones_rot == 1]-img_rot1[ones_rot == 1])*(img2[ones_rot == 1]-img_rot2[ones_rot == 1]))/n
  }
  return(var_ang_cr)
}

# Angular Pseudo Cross-Variogram Estimator
PseudoVarAngCr <- function(img1, img2, sep = 1){
  if(180%%sep != 0){
    stop("sep parameter must be divisor of 180")
  }
  N <- 180/sep
  pseudo_var_ang_cr <- numeric(N)
  l <- dim(img1)[1]
  im1 <- as.cimg(img1)
  im2 <- as.cimg(img2)
  ones <- matrix(1, l, l)
  im_ones <- as.cimg(ones)
  for(i in 1:N){
    im_rot1 <- imrotate(im1, sep*i)
    im_rot2 <- imrotate(im2, sep*i)
    img_rot1 <- as.array(im_rot1)[,,1,1]
    img_rot2 <- as.array(im_rot2)[,,1,1]
    im_ones_rot <- imrotate(im_ones, sep*i)
    ones_rot <- as.array(im_ones_rot)[,,1,1]
    l_rot <- dim(ones_rot)[1]
    w_rot <- dim(ones_rot)[2]
    l_dif <- l_rot - l
    w_dif <- w_rot - l
    img_rot1 <- img_rot1[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    img_rot2 <- img_rot2[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    ones_rot <- ones_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
    n <- sum(ones_rot == 1)
    pseudo_var_ang_cr[i] <- sum((img_rot1[ones_rot == 1]-img2[ones_rot == 1])^2)/n
  }
  return(pseudo_var_ang_cr)
}