#Libraries
library(rstudioapi)

# Work path
path <- dirname(getActiveDocumentContext()$path)
source(paste(path,"/functions.R", sep=""))

#################################################################
######### ANALYSIS OF IMAGES WITH DISTORTIONS (TID2013) #########
#################################################################

# The reference and distorted images are opened and rescaled to 300x300.
for(i in 1:25){
  cat("Image number: ", i, " \n")
  if(i < 10){
    eval(parse(text = paste("img", i, " <- scaleImage(openImage('", path,"/tid2013/reference_images/I0", i, ".bmp'), 300)")))
    for(j in 1:24){
      if(j < 10){
        eval(parse(text = paste("img", i, "_d_", j, " <- scaleImage(openImage('", path,"/tid2013/distorted_images/i0", i, "_0", j, "_5.bmp'), 300)")))
      }
      else{
        eval(parse(text = paste("img", i, "_d_", j, " <- scaleImage(openImage('", path,"/tid2013/distorted_images/i0", i, "_", j, "_5.bmp'), 300)")))
      }
    }
  }
  else{
    eval(parse(text = paste("img", i, " <- scaleImage(openImage('", path,"/tid2013/reference_images/I", i, ".bmp'), 300)")))
    for(j in 1:24){
      if(j < 10){
        eval(parse(text = paste("img", i, "_d_", j, " <- scaleImage(openImage('", path,"/tid2013/distorted_images/i", i, "_0", j, "_5.bmp'), 300)")))
      }
      else{
        eval(parse(text = paste("img", i, "_d_", j, " <- scaleImage(openImage('", path,"/tid2013/distorted_images/i", i, "_", j, "_5.bmp'), 300)")))
      }
    }
  }
}

# The rotation angles to be used are defined.
angles <- c(3, 30, 45)

# Angular Variograms are obtained from the reference images.
for(i in 1:25){
  cat("Variograma Angular nro: ", i, " \n")
  eval(parse(text = paste("var_ang_", i, " <- VarAng(img", i, ")")))
}

### The Angular Cross-Variograms for the previously defined angles are obtained.
for(ang in angles){
  for(i in 1:25){
    cat("Image nro: ", i, " \n")
    for(j in 1:24){
      cat("   Distortion nro: ", j, " \n")
      eval(parse(text = paste("var_ang_cr_", i, "_", j, "_", ang," <- VarAngCr(img", i, ", rotateImage(img", i, "_d_", j, ", ", ang,"))", sep="")))
    }
  }
  eval(parse(text = paste("var_ang_cr_", ang," <- c()", sep="")))
  for(i in 1:25){
    for(j in 1:24){
      eval(parse(text = paste("var_ang_cr_", ang," <- cbind(var_ang_cr_", ang,", var_ang_cr_", i, "_", j, "_", ang,")", sep="")))
    }
  }
}

## The mean and standard deviation of the results are calculated, grouping by images and by distortion.
vac_res_x_img <- c()
vac_res_x_dist <- c()

for(ang in angles){
  cat("Angle: ", ang, "\n")
  eval(parse(text = paste("ang_min_", ang,"_vac <- c()", sep="")))
  for(i in 1:600){
    cat("   Image number:",i,"\n")
    eval(parse(text = paste("ind <- which(sapply(var_ang_cr_", ang,"[,i], function(x) x == min(var_ang_cr_", ang,"[,i])))", sep="")))
    eval(parse(text = paste("ang_min_", ang,"_vac[i] <- rbind(as.integer(ind))", sep="")))
  }
  # By image
  eval(parse(text = paste("ang_min_", ang,"_vac_x_img <- c()", sep="")))
  for(i in 1:25){
    eval(parse(text = paste("ang_min_", ang,"_vac_x_img <- rbind(ang_min_",ang,"_vac_x_img, cbind('VarAngCr', 'img', ",ang,",mean(ang_min_",ang,"_vac[(24*i-23):(24*i)]), sd(ang_min_",ang,"_vac[(24*i-23):(24*i)])))", sep="")))
  }
  eval(parse(text = paste("vac_res_x_img <- rbind(vac_res_x_img, ang_min_",ang,"_vac_x_img)", sep="")))
  # By distortion
  eval(parse(text = paste("ang_min_",ang,"_vac_x_dist <- c()", sep="")))
  for(i in 0:23){
    eval(parse(text = paste("ang_min_",ang,"_vac_x_dist <- rbind(ang_min_",ang,"_vac_x_dist, cbind('VarAngCr', 'dist', ",ang,",mean(ang_min_",ang,"_vac[(1:600)%%24==i]), sd(ang_min_",ang,"_vac[(1:600)%%24==i])))", sep="")))
  }
  eval(parse(text = paste("vac_res_x_dist <- rbind(vac_res_x_dist, ang_min_",ang,"_vac_x_dist)", sep="")))
}

#-----------------------------------------------------------------------------------------------------------#

### The Angular Pseudo Cross-Variograms for the previously defined angles are obtained.
for(ang in angles){
  for(i in 1:25){
    cat("Image nro: ", i, " \n")
    for(j in 1:24){
      cat("   Distortion nro: ", j, " \n")
      eval(parse(text = paste("pseudo_var_ang_cr_", i, "_", j, "_", ang," <- PseudoVarAngCr(img", i, ", rotateImage(img", i, "_d_", j, ", ", ang,"))", sep="")))
    }
  }
  eval(parse(text = paste("pseudo_var_ang_cr_", ang," <- c()", sep="")))
  for(i in 1:25){
    for(j in 1:24){
      eval(parse(text = paste("pseudo_var_ang_cr_", ang," <- cbind(pseudo_var_ang_cr_", ang,", pseudo_var_ang_cr_", i, "_", j, "_", ang,")", sep="")))
    }
  }
}

## The mean and standard deviation of the results are calculated, grouping by images and by distortion.
pvac_res_x_img <- c()
pvac_res_x_dist <- c()

for(ang in angles){
  cat("Angle: ", ang, "\n")
  eval(parse(text = paste("ang_min_", ang,"_pvac <- c()", sep="")))
  for(i in 1:600){
    cat("   Image number:",i,"\n")
    eval(parse(text = paste("ind <- which(sapply(pseudo_var_ang_cr_", ang,"[,i], function(x) x == min(pseudo_var_ang_cr_", ang,"[,i])))", sep="")))
    eval(parse(text = paste("ang_min_", ang,"_pvac[i] <- rbind(as.integer(ind))", sep="")))
  }
  # By image
  eval(parse(text = paste("ang_min_", ang,"_pvac_x_img <- c()", sep="")))
  for(i in 1:25){
    eval(parse(text = paste("ang_min_", ang,"_pvac_x_img <- rbind(ang_min_",ang,"_pvac_x_img, cbind('PseudoVarAngCr', 'img', ",ang,", mean(ang_min_",ang,"_pvac[(24*i-23):(24*i)]), sd(ang_min_",ang,"_pvac[(24*i-23):(24*i)])))", sep="")))
  }
  eval(parse(text = paste("pvac_res_x_img <- rbind(pvac_res_x_img, ang_min_",ang,"_pvac_x_img)", sep="")))
  # By distortion
  eval(parse(text = paste("ang_min_",ang,"_pvac_x_dist <- c()", sep="")))
  for(i in 0:23){
    eval(parse(text = paste("ang_min_",ang,"_pvac_x_dist <- rbind(ang_min_",ang,"_pvac_x_dist, cbind('PseudoVarAngCr', 'dist', ",ang,", mean(ang_min_",ang,"_pvac[(1:600)%%24==i]), sd(ang_min_",ang,"_pvac[(1:600)%%24==i])))", sep="")))
  }
  eval(parse(text = paste("pvac_res_x_dist <- rbind(pvac_res_x_dist, ang_min_",ang,"_pvac_x_dist)", sep="")))
}

# The results are saved in a dataframe, and formatting is applied.
summary_results <- data.frame(rbind(vac_res_x_img, vac_res_x_dist, pvac_res_x_img, pvac_res_x_dist))
summary_results <- summary_results %>% mutate(X3 = as.integer(X3), X4 = as.double(X4), X5 = as.double(X5))
names(summary_results) <- c("function", "type_agg", "angle", "mean", "sd")

# The dataframe with the results is saved to a .csv file.
if(!file.exists(paste(path, "/RESULTS_REAL_IMAGES", sep=""))){
  dir.create(paste(path, "/RESULTS_REAL_IMAGES", sep=""))
}
write.csv(summary_results, paste(path,"/RESULTS_REAL_IMAGES/summary_results_real_img.csv", sep=''), row.names = FALSE)