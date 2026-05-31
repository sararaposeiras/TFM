
# ---------------------------------------------------------------
# SIMULACION RESPUESTA ESCENARIOS 1, 2 Y 3
# ---------------------------------------------------------------

# Librerias necesarias:
library(fields)
library(INLA)
library(ggplot2)
library(patchwork)

# Semilla para reproducibilidad:
set.seed(1423)  


# SIMULACION ESCENARIO 1 #######################################################
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

# Campo espacial residual (Matérn):
cov_error <- matern.image.cov(grid = grid_list,
                              theta = 15,
                              smoothness = 0.8,
                              setup = TRUE)
error_field <- sim.rf(cov_error)
error_matrix <- matrix(error_field, nrow = dims, ncol = dims)

# Error iid pequeño:
sigma_iid <- 0.3
error_iid <- matrix(rnorm(dims*dims, 0, sigma_iid), nrow = dims, ncol = dims)

# Respuesta final:
y_matrix <- error_matrix + error_iid



#---------------------------------------------------
# 3. Plots para inspección visual
# --------------------------------------------------
# Paletas de colores:
cols_main <- hcl.colors(100, "Spectral", rev = TRUE)

df_y <- expand.grid(x = grid_list$x, y = grid_list$y)
df_y$value <- as.vector(y_matrix)

# Respuesta:
p1 <- ggplot(df_y, aes(x, y, fill = value)) +
  geom_raster() +
  coord_fixed(ratio = 0.65) +
  scale_fill_gradientn(colours = cols_main) +
  labs(title = expression(atop("Escenario 1", y(s) == alpha(s) + epsilon(s))),
       x = "X", y = "Y", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank())
  




# SIMULACION ESCENARIO 2 #######################################################
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

# Error iid pequeño:
sigma_iid <- 0.3
error_iid <- matrix(rnorm(dims*dims, 0, sigma_iid), nrow = dims, ncol = dims)

# Respuesta final:
y_matrix <- eta.real_matrix + error_iid



#---------------------------------------------------
# 3. Plots para inspección visual
# --------------------------------------------------
df_y <- expand.grid(x = grid_list$x, y = grid_list$y)
df_y$value <- as.vector(y_matrix)

# Respuesta:
p2 <- ggplot(df_y, aes(x, y, fill = value)) +
  geom_raster() +
  coord_fixed(ratio = 0.65) +
  scale_fill_gradientn(colours = cols_main) +
  labs(title = expression(atop("Escenario 2", y(s) == beta(s) %.% X+ epsilon(s))),
       x = "X", y = "Y", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank())






# SIMULACION ESCENARIO 3 #######################################################
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

# Campo espacial residual (Matérn):
cov_error <- matern.image.cov(grid = grid_list,
                              theta = 15,
                              smoothness = 0.8,
                              setup = TRUE)
error_field <- sim.rf(cov_error)
error_matrix <- matrix(error_field, nrow = dims, ncol = dims)

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



#---------------------------------------------------
# 3. Plots para inspección visual
# --------------------------------------------------
df_y <- expand.grid(x = grid_list$x, y = grid_list$y)
df_y$value <- as.vector(y_matrix)

# Respuesta:
p3 <- ggplot(df_y, aes(x, y, fill = value)) +
  geom_raster() +
  coord_fixed(ratio = 0.65) +
  scale_fill_gradientn(colours = cols_main) +
  labs(title = expression(atop("Escenario 3", y(s) == alpha(s) + beta(s) %.% X + epsilon(s))),
       x = "X", y = "Y", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid = element_blank())



p1 + p2 + p3 +
  plot_layout(ncol = 3, byrow = TRUE)
