
# -------------------------------------------------
# GRAFICA SIMULACION CAMPOS ESPACIALES 
# -------------------------------------------------
# Librerias necesarias
library(fields)

# Semilla para reproducibilidad
set.seed(1423) 

# Malla de puntos
dims <- 101 #dimensiones
n <- dims^2
grid_list <- list(x = seq(0, 100, length = dims),
                  y = seq(0, 100, length = dims))

# Coordenadas
lon <- rep(grid_list$x, times = dims)
lat <- rep(grid_list$y, each = dims)

# Beta espacial (Matérn)
cov_beta <- matern.image.cov(grid = grid_list,
                             theta = 10,       
                             smoothness = 1.2, 
                             setup = TRUE)

beta_field <- sim.rf(cov_beta) * 0.5 
beta_matrix <- matrix(beta_field, nrow = dims, ncol = dims)

# Covariable X (gradiente + ruido local)
X_det <- scale(lon + lat)           
X_noise <- rnorm(dims*dims, 0, 0.5) 
X_matrix <- matrix(X_det + X_noise, nrow = dims, ncol = dims)

# Efecto combinado beta*X
eta_matrix <- beta_matrix * X_matrix

# Error espacial (Matérn)
cov_error <- matern.image.cov(grid = grid_list,
                              theta = 15,
                              smoothness = 0.8,
                              setup = TRUE)
error_field <- sim.rf(cov_error)
error_matrix <- matrix(error_field, nrow = dims, ncol = dims)

# Recalibrar beta para equilibrar varianzas
var_eta <- var(as.vector(eta_matrix))
var_error <- var(as.vector(error_matrix))
factor_beta <- sqrt(var_error / var_eta)
beta_matrix <- beta_matrix * factor_beta
eta_matrix <- beta_matrix * X_matrix

# Error iid pequeño
sigma_iid <- 0.3
error_iid <- matrix(rnorm(dims*dims, 0, sigma_iid), nrow = dims, ncol = dims)

# Respuesta final
y_matrix <- eta_matrix + error_matrix + error_iid



# -------------------------------------------------
# 9. Plots para inspección visual
# -------------------------------------------------
cols_main <- hcl.colors(100, "Spectral", rev = TRUE)
cols_err <- colorRampPalette(c("blue2", "white", "red2"))(100)

par(mfrow=c(2,2))
image.plot(grid_list$x, grid_list$y, beta_matrix,
           col=cols_main, main = expression("Coeficiente espacialmente variable " * beta(s)), xlab="X", ylab="Y")

image.plot(grid_list$x, grid_list$y, X_matrix,
           col=cols_main, main = expression("Covariable espacial " * X(s)), xlab="X", ylab="Y")

image.plot(grid_list$x, grid_list$y, eta_matrix,
           col=cols_main, main = expression(atop("Efecto espacial asociado a la covariable",
                                                 
                                                 eta(s) == beta(s) %.% X(s))), xlab="X", ylab="Y")

image.plot(grid_list$x, grid_list$y, error_matrix,
           col=cols_main, main = expression("Efecto espacial residual " * alpha(s)), xlab="X", ylab="Y")

