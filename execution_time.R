#Libraries
library(rstudioapi)

#Work path
path <- dirname(getActiveDocumentContext()$path)
source(paste(path,"/functions.R", sep=""))

####################################
########## EXECUTION TIME ##########
####################################

set.seed(123)

times <- c()
l <- 100
x <- 50:l

for(n in x){
  rd_matrix <- matrix(runif(n^2), nrow=n, ncol=n)
  rot_matrix <- rotateImage(rd_matrix, 60)
  t <- system.time({
    var_ang_cr <- VarAngCr(rd_matrix, rot_matrix)
  })
  times <- rbind(times, t[3])
}

data <- data.frame(x, times)

# The dataframe with the results is saved to a .csv file.
if(!file.exists(paste(path, "/EXECUTION_TIME", sep=""))){
  dir.create(paste(path, "/EXECUTION_TIME", sep=""))
}
write.csv(data, paste(path,"/EXECUTION_TIME/execution_time.csv", sep=''), row.names = FALSE)

# Linea Model y~beta*(x^2)
model <- lm(times~I(x^2)-1, data = data)

# Graphics
plot(x, times, xlab = 'Length of the image', ylab='Time', title('Execution Time AngCrVar Algorithm'), pch=1)
lines(x, model$fitted.values, col='red', lwd=4)
legend('topleft', legend = c('Empirical data','Fitted curve'), col = c('black', 'red'), pch = c(1, NA), lty = c(NA, 1), lwd = c(NA, 4))
