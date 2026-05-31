
# ----------------------------------------------------------------------
# Gráfica puntos muestreados segun el esquema de muestreo seleccionado
# ----------------------------------------------------------------------

# Librerias necesarias
library(fields)

# Semilla para reproducibilidad
set.seed(1423) 

########### SIMULACION #################################

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



########### MUESTREO ALEATORIO #################################
n_samples <- 500      # número de puntos muestreados

sample_aleatorio <- sample(1:n, n_samples, replace = FALSE)

# Puntos seleccionados:
cols_main <- hcl.colors(100, "Spectral", rev = TRUE)
par(mfrow=c(1,3))
image.plot(grid_list$x, grid_list$y, y_matrix,
           col = cols_main, main = expression(atop("Muestreo aleatorio",
                                                   gamma == 0)), xlab = "X", ylab = "Y")
points(lon[sample_aleatorio], lat[sample_aleatorio], col = "black", cex = 0.5, pch = 4)



########### MUESTREO PREFERENCIAL (alpha=1) #################################
alpha <- 1 # sesgo de selección

# Variable auxiliar positiva para definir preferencia de muestreo
# (como nuestra variable toma valores negativos da problemas)
B_pref <- y_matrix - min(y_matrix) + 1e-6

# Probabilidades de selección
prob_select <- B_pref^alpha / sum(B_pref^alpha)

# Muestreo
sample_aleatorio <- sample(1:n, n_samples, replace = FALSE, prob = prob_select)

# Puntos seleccionados:
image.plot(grid_list$x, grid_list$y, y_matrix,
           col = cols_main, main = expression(atop("Muestreo preferencial moderado",
                                                   gamma == 1)), xlab = "X", ylab = "Y")
points(lon[sample_aleatorio], lat[sample_aleatorio], col = "black", cex = 0.5, pch = 4)




########### MUESTREO ALEATORIO (alpha=5) #################################
alpha <- 5 # sesgo de selección

# Probabilidades de selección
prob_select <- B_pref^alpha / sum(B_pref^alpha)

# Muestreo
sample_aleatorio <- sample(1:n, n_samples, replace = FALSE, prob = prob_select)

# Puntos seleccionados:
image.plot(grid_list$x, grid_list$y, y_matrix,
           col = cols_main, main = expression(atop("Muestreo preferencial fuerte",
                                                   gamma == 5)), xlab = "X", ylab = "Y")
points(lon[sample_aleatorio], lat[sample_aleatorio], col = "black", cex = 0.5, pch = 4)
par(mfrow=c(1,1))

