# Libraries
library(fields)
library(MASS)
library(writexl)
library(progress)
library(rstudioapi)

########################################
######### Covariance Functions #########
########################################

# Exponential
cov_function_exp <- function(h, a=1, scale=1) {
  return(scale * exp(-a * h))
}

# Spherical
cov_function_spherical <- function(h, a=1) {
  cov_values <- ifelse(h <= a, 
                       1 - 1.5 * (h / a) + 0.5 * (h / a)^3,
                       0)
  return(cov_values)
}

# Gaussian
cov_function_gaussian <- function(h, a=1) {
  exp(- (h^2) / (2 * a^2))
}

# New covariance functions can be added

####### Function to simulate images #######

simulateImages <- function(nx=50, ny=50, N=100, covar, name_output_file){
  # Create a progress bar
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  all_images <- c()
  for(i in 1:N){
    set.seed(i)
    eval(parse(text = paste("sim_img_",i," <- mvrnorm(n = 1, mu = rep(0, nx * ny), Sigma = ",covar, ")", sep = "")))
    eval(parse(text = paste("all_images <- cbind(all_images, sim_img_",i,")", sep = "")))
    pb$tick()
  }
  if(!file.exists(paste(path, "/SIMULATED_IMAGES", sep=""))){
    dir.create(paste(path, "/SIMULATED_IMAGES", sep=""))
  }
  write.csv(all_images, paste(path,"/SIMULATED_IMAGES/",name_output_file,".csv", sep=''), row.names = FALSE)
  
  return(all_images)
}




########################################
######### SIMULATION OF IMAGES #########
########################################

path <- dirname(getActiveDocumentContext()$path)

# Number of simulated images
N = 10

# Grid parameters
nx <- 50  # Number of pixels along the x axis
ny <- 50  # Number of pixels along the y axis

# Create a grid
x <- seq(0, 1, length = nx)
y <- seq(0, 1, length = ny)
grid <- expand.grid(x = x, y = y)

# Name of covariance matrix
covar <- "cov_matrix"

# Calculate the covariance matrix
distances <- as.matrix(dist(grid))
cov_matrix <- cov_function_exp(distances)

# N images with size nx X ny are simulated with given covariance structure.
sim_images <- simulateImages(nx, ny, N, covar, "sim_img_exp")


