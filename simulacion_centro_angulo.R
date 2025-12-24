## Ver archivos anteriores para crear las imagenes

# Libraries
library(fields)
library(MASS)
library(writexl)
library(progress)
library(rstudioapi)
library(dplyr)
library(imager)
library(xtable)

## Modificación de funciones
rotateImage2 <- function(img, ang, centre=NULL){
  l <- dim(img)[1]
  im <- as.cimg(img)
  if(is.null(centre)){ im_rot <- imrotate(im, ang) }else{
    im_rot <- imrotate(im, ang, cx=centre[1], cy=centre[2] )
  }
  img_rot <- as.array(im_rot)[,,1,1]
  l_rot <- dim(img_rot)[1]
  w_rot <- dim(img_rot)[2]
  l_dif <- l_rot - l
  w_dif <- w_rot - l
  img_rot <- img_rot[(l_dif%/%2+1):(l_rot-(l_dif%/%2+l_dif%%2)),(w_dif%/%2+1):(w_rot-(w_dif%/%2+w_dif%%2))]
  return(img_rot)
}
MeanStdResults2 <- function(func, ang, images, centre0=NULL, centre1=NULL){
  N <- length(images)
  ang_min <- c()
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  for(i in 1:N){
    res <- func(images[[i]], rotateImage2(images[[i]], ang, centre=centre0), centre=centre1)
    ind <- which(sapply(res, function(x) x == min(res)))
    print(ind)
    ang_min <- c(ang_min, ind)
    pb$tick()
  }
  return(c(angle=ang, avg=mean(ang_min), std=sd(ang_min)))
}
MeanStdResults3 <- function(func, ang, images, centre0=NULL, sep=0.25, angle.max=90){
  require(parallel)
  N <- length(images)
  ang_min <- c()
  cent_min <- c()
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  d_im <- dim(images[[1]])
  centre1 <- expand.grid(seq(1,d_im[1],by=1), seq(1,d_im[2],by=1))
  angulo1 <- (1:(90/sep))*sep
  M=nrow(centre1)
  RES <- list()
  for(i in 1:N){
    print(paste("\n iteracion",i))
    RES1 <- do.call(cbind, mclapply(1:M, function(j) {
      res <- func( images[[i]], rotateImage2(images[[i]], ang, centre = centre0),
                   sep=sep, centre = as.numeric(centre1[j, ]), angle.max=angle.max)
      res$var_ang
    }, mc.cores = detectCores() - 1))  # usa todos los núcleos menos 1
    
    print(dim(RES1))
    RES[[i]] <- RES1
    ind_angulos <- apply(RES1,2,which.min) # angulo minimo para cada centro
    ind_centro <- which.min(apply(RES1,2,min)) # centro minimo
    ind_ang <- ind_angulos[ind_centro] 
    
    ang_min <- c(ang_min, angulo1[ind_ang] )
    cent_min <- rbind( cent_min, centre1[ind_centro, ])
    pb$tick()
  }
  return( list(all=RES,
    resumen=c(angle=ang, 
              avg_angle=mean(ang_min), 
              std_angle=sd(ang_min), 
              rmse_angle=sqrt(mean((ang_min-ang)^2)),
              mae_angle=mean(abs(ang_min-ang)),
              centre1=centre0[1], 
              avg_centre1=mean(cent_min[,1]), 
              std_centre1=sd(cent_min[,1]), 
              rmse_centre1=sqrt(mean((cent_min[,1]-centre0[1])^2)),
              mae_centre1=mean(abs(cent_min[,1]-centre0[1])),
              centre2=centre0[2], 
              avg_centre2=mean(cent_min[,2]), 
              std_centre2=sd(cent_min[,2]), 
              rmse_centre2=sqrt(mean((cent_min[,2]-centre0[2])^2)),
              mae_centre2=mean(abs(cent_min[,2]-centre0[2])),
              rmse_centre=sqrt(mean((cent_min[,1]-centre0[1])^2+(cent_min[,2]-centre0[2])^2)),
              mae_centre=sqrt(mean(abs(cent_min[,1]-centre0[1])+abs(cent_min[,2]-centre0[2]) )) ) 
    ) )
}

resumen <- function(RES, param0=list(angle0, centre0)){
  N=length(RES)
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  centre1 <- expand.grid(seq(1,50,by=1), seq(1,50,by=1))
  sep=0.5; angulo1 <- (1:(90/sep))*sep
  
  ang_min <- c()
  cent_min <- c()
  for(i in 1:N){
    RES1 <- RES[[i]][1:180, ]
    ind_angulos <- apply(RES1,2,which.min) # angulo minimo para cada centro
    ind_centro <- which.min(apply(RES1,2,min)) # centro minimo
    ind_ang <- ind_angulos[ind_centro] 
    
    ang_min <- c(ang_min, angulo1[ind_ang] )
    cent_min <- rbind( cent_min, centre1[ind_centro, ])
    pb$tick()
  }
  
  return(data.frame(true_angle=param0$angle0, 
                    mean_angle=mean(ang_min), 
                    rmse_angle=sqrt(mean((ang_min-param0$angle0)^2)), 
                    true_centre1=param0$centre0[1], 
                    mean_centre1=mean(cent_min[,1]), 
                    true_centre2=param0$centre0[2], 
                    mean_centre2=mean(cent_min[,2]),
                    rmse_centre=sqrt(mean((cent_min[,1]-param0$centre0[1])^2+(cent_min[,2]-param0$centre0[2])^2)) ) )
}

# falta modificar VarAngCr2 para incluir el centro en la estimación
VarAngCr_v2 <- function(img1, img2, sep = 0.5, centre=NULL){
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
    if(is.null(centre)){
      im_rot1 <- imrotate(im1, sep*i)
      im_rot2 <- imrotate(im2, sep*i)
      im_ones_rot <- imrotate(im_ones, sep*i) }else{
        im_rot1 <- imrotate(im1, sep*i, centre[1], centre[2])
        im_rot2 <- imrotate(im2, sep*i, centre[1], centre[2])
        im_ones_rot <- imrotate(im_ones, sep*i, centre[1], centre[2]) }
    img_rot1 <- as.array(im_rot1)[,,1,1]
    img_rot2 <- as.array(im_rot2)[,,1,1]
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
PseudoVarAngCr_v2 <- function(img1, img2, sep = 0.5, centre=NULL, angle.max=90){
  if(angle.max%%sep != 0){
    stop("sep parameter must be divisor of angle.max")
  }
  N <- angle.max/sep
  pseudo_var_ang_cr <- numeric(N)
  l <- dim(img1)[1]
  im1 <- as.cimg(img1)
  im2 <- as.cimg(img2)
  ones <- matrix(1, l, l)
  im_ones <- as.cimg(ones)
  for(i in 1:N){
    if(is.null(centre)){
      im_rot1 <- imrotate(im1, sep*i)
      im_rot2 <- imrotate(im2, sep*i)
      im_ones_rot <- imrotate(im_ones, sep*i) }else{
        im_rot1 <- imrotate(im1, sep*i, centre[1], centre[2])
        im_rot2 <- imrotate(im2, sep*i, centre[1], centre[2])
        im_ones_rot <- imrotate(im_ones, sep*i, centre[1], centre[2]) }
    img_rot1 <- as.array(im_rot1)[,,1,1]
    img_rot2 <- as.array(im_rot2)[,,1,1]
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
  return(list(var_ang=pseudo_var_ang_cr, ang=sep*(1:N) ) )
}

simulateImages2 <- function(nx=50, ny=50, N=100, covar, name_output_file, seed=0){
  # Create a progress bar
  pb <- progress_bar$new(
    format = "  [:bar] :percent :eta",
    total = N,
    width = 100
  )
  set.seed(seed)
  all_images <- t(mvrnorm(n = N, mu = rep(0, nx * ny), Sigma = covar))
  pb$tick()
  
  if(!file.exists(paste(path, "/SIMULATED_IMAGES", sep=""))){
    dir.create(paste(path, "/SIMULATED_IMAGES", sep=""))
  }
  write.csv(all_images, paste(path,"/SIMULATED_IMAGES/",name_output_file,".csv", sep=''), row.names = FALSE)
  
  return(all_images)
}

# funciones auxiliares

minmax <- function(x){ 
  a=min(x, na.rm = TRUE)
  b=max(x, na.rm = TRUE)
  x <- (x-a)/(b-a)
  return(x)
  }

# distance matrix, previamente calculada
distances[1:5,1:5]


####------------------------------------------------------------------------####
## simulacion de imágenes exponenciales
cov_matrix_exp <- cov_function_exp(distances, a=3/0.25) # previamente calculada distances
sim_images_exp <- simulateImages2(nx=50, ny=50, N=1000, covar=cov_matrix_exp, "sim_img_exp", seed=0)
base::save.image()

images_matrix_exp <- c()
for (i in 1:ncol(sim_images_exp)) {
  image_vector <- as.vector(sim_images_exp[, i])
  image_matrix <- matrix(image_vector, nrow = 50)
  images_matrix_exp[[i]] <- image_matrix
}

# Rotacion 30 grados
images_exp_rotated30 <- list()
for(j in 1:length(images_matrix_exp)){
  images_exp_rotated30[[j]] <- rotateImage(images_matrix_exp[[j]], 30)
}


sep=0.25
length(sep*(0:(90/sep))[-1])

## Estimación pseudo-variograma cruzado
time_exp <- system.time({
  res_Pseudo_VarAng_exp_ang30_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_exp, centre0=c(20,10)); 
  res_Pseudo_VarAng_exp_ang15_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_exp, centre0=c(20,10)); 
  res_Pseudo_VarAng_exp_ang30_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_exp, centre0=c(5,40));
  res_Pseudo_VarAng_exp_ang15_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_exp, centre0=c(5,40));  
  })
time_exp/3600 # en horas
base::save.image()

par(mfrow=c(2,2))
sep=0.25
for(j in 1:4){
  matplot(sep*(1:(90/sep)), res_Pseudo_VarAng_exp_ang15_cent20_10$all[[j]], type="l", xlab="angle")
  abline(v=c(15.1, res_Pseudo_VarAng_exp_ang15_cent20_10$resumen[2]), 
         h=min(res_Pseudo_VarAng_exp_ang15_cent20_10$all[[j]], na.rm=TRUE), lty=2, lwd=2, col=c(1,2,3)) }

####---------------------------------------####
# simulacion de imágenes gaussianas
cov_matrix_gau <- cov_function_gaussian(distances, a=0.25/sqrt(6) ) # previamente calculada distances
sim_images_gau <- simulateImages2(nx=50, ny=50, N=1000, covar=cov_matrix_gau, "sim_img_gau", seed=1)
base::save.image()

images_matrix_gau <- c()
for (i in 1:ncol(sim_images_gau)) {
  image_vector <- as.vector(sim_images_gau[, i])
  image_matrix <- matrix(image_vector, nrow = 50)
  images_matrix_gau[[i]] <- image_matrix
}


## Pseudo variograma cruzado en modelos gaussianos
time_gau <- system.time({
  res_Pseudo_VarAng_gau_ang30_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_gau, centre0=c(20,10)); 
  res_Pseudo_VarAng_gau_ang15_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_gau, centre0=c(20,10)); 
  res_Pseudo_VarAng_gau_ang30_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_gau, centre0=c(5,40));
  res_Pseudo_VarAng_gau_ang15_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_gau, centre0=c(5,40)) }); 
time_gau/3600
base::save.image()

par(mfrow=c(2,2))
sep=0.5
for(j in 1:4){
  matplot(sep*(1:(180/sep)), res_Pseudo_VarAng_gau_ang15_cent20_10$all[[j]][,1:10], type="l", xlab="angle")
  abline(v=c(15.1, res_Pseudo_VarAng_gau_ang15_cent20_10$resumen[2]), 
         h=min(res_Pseudo_VarAng_gau_ang15_cent20_10$all[[j]], na.rm=TRUE), lty=2, lwd=2, col=c(1,2,3)) }

##


####---------------------------------------####
# simulacion de imágenes sphericas
cov_matrix_sph <- cov_function_spherical(distances, a=0.308109) # previamente calculada distances
sim_images_sph <- simulateImages2(nx=50, ny=50, N=1000, covar=cov_matrix_gau, "sim_img_sph", seed=2)


images_matrix_sph <- c()
for (i in 1:ncol(sim_images_sph)) {
  image_vector <- as.vector(sim_images_sph[, i])
  image_matrix <- matrix(image_vector, nrow = 50)
  images_matrix_sph[[i]] <- image_matrix
}


## Pseudo variograma cruzado en modelos esfericos
time_sph <- system.time({
  res_Pseudo_VarAng_sph_ang30_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_sph, centre0=c(20,10)); 
  res_Pseudo_VarAng_sph_ang15_cent20_10 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_sph, centre0=c(20,10)); 
  res_Pseudo_VarAng_sph_ang30_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 30, images_matrix_sph, centre0=c(5,40));  
  res_Pseudo_VarAng_sph_ang15_cent5_40 <- MeanStdResults3(PseudoVarAngCr_v2, 15.1, images_matrix_sph, centre0=c(5,40)) });
time_sph/3600
base::save.image()

par(mfrow=c(2,2))
sep=0.25
for(j in 1:4){
  matplot(sep*(1:(90/sep)), res_Pseudo_VarAng_sph_ang15_cent20_10$all[[j]][,1:200], type="l", xlab="angle", ylim=c(0,4))
  abline(v=c(15.1, res_Pseudo_VarAng_sph_ang15_cent20_10$resumen[2]), 
         h=min(res_Pseudo_VarAng_sph_ang15_cent20_10$all[[j]], na.rm=TRUE), lty=2, lwd=2, col=c(1,2,3)) }

### Resumen

resumen_ang_centre <- data.frame(covmodel=rep(c('Exp', 'Gau', 'Sph'), each=4),
                               rbind(
                                 res_Pseudo_VarAng_exp_ang15_cent20_10$resumen,
                                 res_Pseudo_VarAng_exp_ang15_cent5_40$resumen,
                                 res_Pseudo_VarAng_exp_ang30_cent20_10$resumen,
                                 res_Pseudo_VarAng_exp_ang30_cent5_40$resumen,
                                 res_Pseudo_VarAng_gau_ang15_cent20_10$resumen,
                                 res_Pseudo_VarAng_gau_ang15_cent5_40$resumen,
                                 res_Pseudo_VarAng_gau_ang30_cent20_10$resumen,
                                 res_Pseudo_VarAng_gau_ang30_cent5_40$resumen,
                                 res_Pseudo_VarAng_sph_ang15_cent20_10$resumen,
                                 res_Pseudo_VarAng_sph_ang15_cent5_40$resumen,
                                 res_Pseudo_VarAng_sph_ang30_cent20_10$resumen,
                                 res_Pseudo_VarAng_sph_ang30_cent5_40$resumen ) )

# colnames(resumen_ang_centre) <- c(colnames(resumen_ang_centre)[-18], "mae_centre") #modificar las columnas en cada conjunto de datos

resumen_ang_centre$centre <- paste0("(", resumen_ang_centre$centre1, ",", resumen_ang_centre$centre2, ")")

res_ang_centre <- resumen_ang_centre[c("covmodel", "angle", "rmse_angle", "mae_angle", "centre", "rmse_centre", "mae_centre")]
res_ang_centre


print(xtable(res_ang_centre), include.rownames = FALSE, booktab=TRUE)



## Guardar los resultados

base::save(res_Pseudo_VarAng_gau_ang15_cent20_10,
     res_Pseudo_VarAng_gau_ang15_cent5_40,
     res_Pseudo_VarAng_gau_ang30_cent20_10,
     res_Pseudo_VarAng_gau_ang30_cent5_40,
     file = "resultados_gaussian.RData")


base::save(res_Pseudo_VarAng_exp_ang15_cent20_10,
     res_Pseudo_VarAng_exp_ang15_cent5_40,
     res_Pseudo_VarAng_exp_ang30_cent20_10,
     res_Pseudo_VarAng_exp_ang30_cent5_40,
     file = "resultados_exponential.RData")

base::save(res_Pseudo_VarAng_sph_ang15_cent20_10,
     res_Pseudo_VarAng_sph_ang15_cent5_40,
     res_Pseudo_VarAng_sph_ang30_cent20_10,
     res_Pseudo_VarAng_sph_ang30_cent5_40,
     file = "resultados_spherical.RData")

#### Ejemplos de realizaciones angulo 30 ####

# Modelos Exponencial
images_exp_rotated30 <- list()
images2_exp_rotated30 <- list()
images3_exp_rotated30 <- list()
for(j in 1:length(images_matrix_exp)){
  images_exp_rotated30[[j]] <- rotateImage(images_matrix_exp[[j]], 30)
  images2_exp_rotated30[[j]] <- rotateImage2(images_matrix_exp[[j]], 30, c(20,10))
  images3_exp_rotated30[[j]] <- rotateImage2(images_matrix_exp[[j]], 30, c(5,40))
}

# Modelos Gaussiano
images_gau_rotated30 <- list()
images2_gau_rotated30 <- list()
images3_gau_rotated30 <- list()
for(j in 1:length(images_matrix_gau)){
  images_gau_rotated30[[j]] <- rotateImage(images_matrix_gau[[j]], 30)
  images2_gau_rotated30[[j]] <- rotateImage2(images_matrix_gau[[j]], 30, c(20,10))
  images3_gau_rotated30[[j]] <- rotateImage2(images_matrix_gau[[j]], 30, c(5,40))
}

# Modelos Spherical
images_sph_rotated30 <- list()
images2_sph_rotated30 <- list()
images3_sph_rotated30 <- list()
for(j in 1:length(images_matrix_sph)){
  images_sph_rotated30[[j]] <- rotateImage(images_matrix_sph[[j]], 30)
  images2_sph_rotated30[[j]] <- rotateImage2(images_matrix_sph[[j]], 30, c(20,10))
  images3_sph_rotated30[[j]] <- rotateImage2(images_matrix_sph[[j]], 30, c(5,40))
}

par(mfcol=c(3,3), mar=c(5,1,1,1))
j=3
# Exponential
image(images_matrix_exp[[j]], xlab="original")
image(images2_exp_rotated30[[j]], xlab="rotación 30 grados con centro=(20,10)")
image(images3_exp_rotated30[[j]], xlab="rotación 30 grados con centro=(5,40)")

# Gaussian
image(images_matrix_gau[[j]], xlab="original")
image(images2_gau_rotated30[[j]], xlab="rotación 30 grados con centro=(20,10)")
image(images3_gau_rotated30[[j]], xlab="rotación 30 grados con centro=(5,40)")

# Spherical
image(images_matrix_sph[[j]], xlab="original")
image(images2_sph_rotated30[[j]], xlab="rotación 30 grados con centro=(20,10)")
image(images3_sph_rotated30[[j]], xlab="rotación 30 grados con centro=(5,40)")

## Guardar imagenes en formato .jpeg

j=1 # realización escogida
realization_images_exp <- list(
  image_original = images_matrix_exp[[j]],
  image_rotated30_C20_10 = images2_exp_rotated30[[j]],
  image_rotated30_C5_40 = images3_exp_rotated30[[j]] )

realization_images_gau <- list(
  image_original = images_matrix_gau[[j]],
  image_rotated30_C20_10 = images2_gau_rotated30[[j]],
  image_rotated30_C5_40 = images3_gau_rotated30[[j]] )

realization_images_sph <- list(
  image_original = images_matrix_sph[[j]],
  image_rotated30_C20_10 = images2_sph_rotated30[[j]],
  image_rotated30_C5_40 = images3_sph_rotated30[[j]] )

ruta_realzation_exp <- paste0(paste0(getwd(),"/SIMULATED_EXPERIMENTS2/"),
                              c("sim_exp_a0_c00.jpeg", 
                                "sim_exp_a30_c20_10.jpeg", 
                                "sim_exp_a30_c5_40.jpeg") )

ruta_realzation_gau <- paste0(paste0(getwd(),"/SIMULATED_EXPERIMENTS2/"),
                              c("sim_gau_a0_c00.jpeg", 
                                "sim_gau_a30_c20_10.jpeg", 
                                "sim_gau_a30_c5_40.jpeg") )

ruta_realzation_sph <- paste0(paste0(getwd(),"/SIMULATED_EXPERIMENTS2/"),
                              c("sim_sph_a0_c00.jpeg", 
                                "sim_sph_a30_c20_10.jpeg", 
                                "sim_sph_a30_c5_40.jpeg") )

for(k in 1:3){
  # Exponential 
  jpeg(ruta_realzation_exp[k])
  par(mfcol=c(1,1), mar=c(0,0,0,0))
  image(realization_images_exp[[k]])
  dev.off()
  
  # Gaussian 
  jpeg(ruta_realzation_gau[k])
  par(mfcol=c(1,1), mar=c(0,0,0,0))
  image(realization_images_gau[[k]])
  dev.off()
  
  # Spherical
  jpeg(ruta_realzation_sph[k])
  par(mfcol=c(1,1), mar=c(0,0,0,0))
  image(realization_images_sph[[k]])
  dev.off()
}
