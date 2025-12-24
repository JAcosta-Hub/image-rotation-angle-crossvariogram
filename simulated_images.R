#Libraries
library(rstudioapi)

#Work path
path <- dirname(getActiveDocumentContext()$path)
source(paste(path,"/functions.R", sep=""))

######### Function that calculates the mean and standard deviation for simulated images #########

MeanStdResults <- function(func, ang, images){
  N <- length(images)
  ang_min <- c()
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  for(i in 1:N){
    res <- func(images[[i]], rotateImage(images[[i]], ang))
    ind <- which(sapply(res, function(x) x == min(res)))
    print(ind)
    ang_min <- c(ang_min, ind)
    pb$tick()
  }
  return(c(angle=ang, avg=mean(ang_min), std=sd(ang_min)))
}

#############################
########## RESULTS ##########
#############################

# The files containing the simulated images are read.
name_file <- "sim_img_exp"
images_list <- read.csv(paste(path,"/SIMULATED_IMAGES/",name_file,".csv", sep=''))

# Convert each column to a 2D array and save to list
images_matrix <- c()
nx <- 50
for (i in 1:ncol(images_list)) {
  image_vector <- as.vector(images_list[, i])
  image_matrix <- matrix(image_vector, nrow = nx)
  images_matrix[[i]] <- image_matrix
}

# The angles and functions to be considered in the calculation are defined.
angles <- c(3, 30, 45)
estimators <- list("VarAngCr" = VarAngCr, "PseudoVarAngCr" =  PseudoVarAngCr)

# The results are obtained.
results <- c()
for(est in names(estimators)){
  for(ang in angles){
    cat("Angle: ", ang, "\n")
    res <- MeanStdResults(estimators[[est]], ang, images_matrix)
    results <- rbind(results, c(estimator = est, res))
  }
}

# The results are saved in a dataframe.
df_results <- data.frame(results)
df_results <- df_results %>% mutate(angle = as.integer(angle), avg = as.double(avg), std = as.double(std))
df_results

# The dataframe with the results is saved to a .csv file.
if(!file.exists(paste(path, "/RESULTS_SIM_IMAGES", sep=""))){
  dir.create(paste(path, "/RESULTS_SIM_IMAGES", sep=""))
}
write.csv(df_results, paste(path,"/RESULTS_SIM_IMAGES/summary_results_sim_img.csv", sep=''), row.names = FALSE)

