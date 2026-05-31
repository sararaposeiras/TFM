
# ---------------------------------------------------------------
# SIMULACION RESPUESTA ESCENARIO 4
# ---------------------------------------------------------------

# Librerias necesarias:
library(fields)
library(INLA)
library(ggplot2)
library(patchwork)

# Semilla para reproducibilidad:
set.seed(1423)  


# SIMULACION RHO = 0.4 #########################################################
# -------------------------
# 1. Dominio espacial
# -------------------------
# Malla de puntos:
dims <- 101 #dimensiones
n <- dims^2
grid_list <- list(x = seq(0, 100, length = dims),
                  y = seq(0, 100, length = dims))

# Coordenadas:
lon <- rep(grid_list$x, times = dims)
lat <- rep(grid_list$y, each = dims)



# --------------------------------------
# 2. Generación de campos espaciales
# --------------------------------------
# Beta espacial (Matérn):
cov_beta.real <- matern.image.cov(grid = grid_list,
                                  theta = 10,       
                                  smoothness = 1.2, 
                                  setup = TRUE)
beta.real_field <- sim.rf(cov_beta.real) * 0.5 
beta.real_matrix <- matrix(beta.real_field, nrow = dims, ncol = dims)

# Covariable X (gradiente + ruido local):
X_det <- scale(lon + lat)           
X_noise <- rnorm(dims*dims, 0, 0.5) 
X_matrix <- matrix(X_det + X_noise, nrow = dims, ncol = dims)

# Efecto combinado beta*X:
eta.real_matrix <- beta.real_matrix * X_matrix

# Campo espacial residual (correlacionado con la covariable):
rho <- 0.4   # controla nivel de correlación
cov_error <- matern.image.cov(grid = grid_list,
                              theta = 15,
                              smoothness = 0.8,
                              setup = TRUE)
error_field <- sim.rf(cov_error)
error_matrix0 <- matrix(error_field, nrow = dims, ncol = dims)
error_matrix <- rho * matrix(eta.real_matrix, nrow = dims, ncol = dims) + (1 - rho) * error_matrix0

# Recalibrar beta para equilibrar varianzas:
var_eta <- var(as.vector(eta.real_matrix))
var_error <- var(as.vector(error_matrix))
factor_beta.real <- sqrt(var_error / var_eta)
beta.real_matrix <- beta.real_matrix * factor_beta.real
eta.real_matrix <- beta.real_matrix * X_matrix

# Comprobación de varianza y rango:
cat("Varianza beta*X:", var(as.vector(eta.real_matrix)), "\n")
cat("Varianza error:", var(as.vector(error_matrix)), "\n")
cat("Rango beta*X:", range(eta.real_matrix), "\n")
cat("Rango error:", range(error_matrix), "\n")

# Error iid pequeño:
sigma_iid <- 0.3
error_iid <- matrix(rnorm(dims*dims, 0, sigma_iid), nrow = dims, ncol = dims)

# Respuesta final:
y_matrix <- eta.real_matrix + error_matrix + error_iid

# Gráfica:
cols_main <- hcl.colors(100, "Spectral", rev = TRUE)
df_y <- expand.grid(x = grid_list$x, y = grid_list$y)
df_y$value <- as.vector(y_matrix)
p03 <- ggplot(df_y, aes(x, y, fill = value)) +
  geom_raster() +
  coord_fixed(ratio = 0.65) +
  scale_fill_gradientn(colours = cols_main) +
  ggtitle(bquote(atop("Respuesta" ~ (rho == 0.4),
                      y(s) == alpha[2](s) + beta(s) %.% X + epsilon(s)))) +
  labs(x = "X", y = "Y", fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )




# SIMULACION RHO = 0.7 #########################################################
set.seed(1423)  # semilla

# -------------------------
# 1. Dominio espacial
# -------------------------
# Malla de puntos:
dims <- 101 #dimensiones
n <- dims^2
grid_list <- list(x = seq(0, 100, length = dims),
                  y = seq(0, 100, length = dims))

# Coordenadas:
lon <- rep(grid_list$x, times = dims)
lat <- rep(grid_list$y, each = dims)



# --------------------------------------
# 2. Generación de campos espaciales
# --------------------------------------
# Beta espacial (Matérn):
cov_beta.real <- matern.image.cov(grid = grid_list,
                                  theta = 10,       
                                  smoothness = 1.2, 
                                  setup = TRUE)
beta.real_field <- sim.rf(cov_beta.real) * 0.5 
beta.real_matrix <- matrix(beta.real_field, nrow = dims, ncol = dims)

# Covariable X (gradiente + ruido local):
X_det <- scale(lon + lat)           
X_noise <- rnorm(dims*dims, 0, 0.5) 
X_matrix <- matrix(X_det + X_noise, nrow = dims, ncol = dims)

# Efecto combinado beta*X:
eta.real_matrix <- beta.real_matrix * X_matrix
# Campo espacial residual (correlacionado con la covariable):
rho <- 0.7   # controla nivel de correlación
cov_error <- matern.image.cov(grid = grid_list,
                              theta = 15,
                              smoothness = 0.8,
                              setup = TRUE)
error_field <- sim.rf(cov_error)
error_matrix0 <- matrix(error_field, nrow = dims, ncol = dims)
error_matrix <- rho * matrix(eta.real_matrix, nrow = dims, ncol = dims) + (1 - rho) * error_matrix0

# Recalibrar beta para equilibrar varianzas:
var_eta <- var(as.vector(eta.real_matrix))
var_error <- var(as.vector(error_matrix))
factor_beta.real <- sqrt(var_error / var_eta)
beta.real_matrix <- beta.real_matrix * factor_beta.real
eta.real_matrix <- beta.real_matrix * X_matrix

# Comprobación de varianza y rango:
cat("Varianza beta*X:", var(as.vector(eta.real_matrix)), "\n")
cat("Varianza error:", var(as.vector(error_matrix)), "\n")
cat("Rango beta*X:", range(eta.real_matrix), "\n")
cat("Rango error:", range(error_matrix), "\n")

# Error iid pequeño:
sigma_iid <- 0.3
error_iid <- matrix(rnorm(dims*dims, 0, sigma_iid), nrow = dims, ncol = dims)

# Respuesta final:
y_matrix <- eta.real_matrix + error_matrix + error_iid

# Gráfica:
df_y <- expand.grid(x = grid_list$x, y = grid_list$y)
df_y$value <- as.vector(y_matrix)
p08 <- ggplot(df_y, aes(x, y, fill = value)) +
  geom_raster() +
  coord_fixed(ratio = 0.65) +
  scale_fill_gradientn(colours = cols_main) +
  ggtitle(bquote(atop("Respuesta" ~ (rho == 0.7),
                      y(s) == alpha[2](s) + beta(s) %.% X + epsilon(s)))) +
  labs(x = "X", y = "Y", fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  )

p03 + p08 +
  plot_layout(ncol = 2, byrow = TRUE)