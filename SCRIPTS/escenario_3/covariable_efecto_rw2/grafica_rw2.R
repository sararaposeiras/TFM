
### GRAFICA RW2 ################################################################

# Librerias necesarias:
library(fields)
library(INLA)
library(ggplot2)
library(patchwork)

# Semilla para reproducibilidad:
set.seed(1423)  

# ------------------------------------------------------------------------------
# ----------------------------     SIMULACIÓN     ------------------------------
# ------------------------------------------------------------------------------

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



# ------------------------------------------------------------------------------
# ----------------------------      MUESTREO      ------------------------------
# ------------------------------------------------------------------------------
n_samples <- 500      # número de puntos muestreados
n_x <- 10     # numero de grupos para la discretizacion

# Muestreo aleatorio:
sample_aleatorio <- sample(1:n, n_samples, replace = FALSE)

# Dataframe con la informacion de las observaciones muestreadas:
dat <- data.frame(
  intercept = 1,
  id = sample_aleatorio,
  lon = lon[sample_aleatorio],
  lat = lat[sample_aleatorio],
  Cov = X_matrix[sample_aleatorio],
  beta.real = beta.real_matrix[sample_aleatorio],
  eta = eta.real_matrix[sample_aleatorio],
  space = error_matrix[sample_aleatorio],
  resp = y_matrix[sample_aleatorio]
)

# RW2: discretizar covariable
X_rw2 <- inla.group(as.numeric(X_matrix), n = n_x)
dat$X_rw2 <- X_rw2[sample_aleatorio]




# ------------------------------------------------------------------------------
# ----------------------------    AJUSTE INLA    -------------------------------
# ------------------------------------------------------------------------------

Loc <- as.matrix(dat[, c("lon", "lat")]) # Localizaciones

mesh <- inla.mesh.2d(Loc, max.edge = c(5, 12), cutoff = 0.5) # Malla

# SPDE para el campo espacial aditivo:
spde.alpha <- inla.spde2.pcmatern(
  mesh = mesh,
  prior.range = c(15, 0.05),
  prior.sigma = c(sd(error_matrix), 0.05)
)

# Índices espaciales:
alpha.index <- inla.spde.make.index("alpha", n.spde = spde.alpha$n.spde)

# Matrices proyectoras (A):
A.alpha <- inla.spde.make.A(mesh = mesh, loc = Loc)

# Stack:
stack <- inla.stack(
  data = list(y = dat$resp),
  effects = list(data.frame(
    intercept = dat$intercept,
    X_rw2 = dat$X_rw2),
    alpha = alpha.index
  ),
  A = list(1, A.alpha),
  tag = "est"
)

# Fórmula: intercepto + alpha(s) + efecto rw2
formula <- y ~ -1 + intercept + f(X_rw2, model = "rw2", constr = TRUE) + 
  f(alpha, model = spde.alpha)

# Ajuste:
model <- inla(formula, family = "gaussian", data = inla.stack.data(stack),
              control.predictor = list(A = inla.stack.A(stack), compute = TRUE), 
              control.compute = list(dic = TRUE, waic = TRUE),
              control.inla = list(strategy = "adaptive", int.strategy = "eb"),
              verbose = FALSE)

# ---------------------------------------------------
# PREDICCION EN TODA LA MALLA
# ---------------------------------------------------
# Dataframe para la predicción:
pred <- data.frame(
  lon = lon,
  lat = lat,
  X = as.vector(X_matrix),
  intercept = 1
)
pred$X_rw2 <- X_rw2 # Añadimos discretizacion para rw2

Loc.pred <- as.matrix(pred[, c("lon", "lat")])  # Localizaciones prediccion

# Matrices proyectoras para la prediccion:
A.alpha.pred <- inla.spde.make.A(mesh = mesh, loc = Loc.pred)

# Stack para la prediccion:
stack.pred <- inla.stack(
  data = list(y = NA),
  effects = list(
    data.frame(intercept = pred$intercept,
               X_rw2 = pred$X_rw2),
    alpha = alpha.index
  ),
  A = list(1, A.alpha.pred),
  tag = "pred"
)

# Stack conjunto: ajuste + predicción:
stack.full <- inla.stack(stack, stack.pred)

# Reajuste del modelo con el stack conjunto:
model.pred <- inla(formula, family = "gaussian", data = inla.stack.data(stack.full),
                   control.predictor = list(A = inla.stack.A(stack.full), compute = TRUE),
                   control.compute = list(dic = TRUE, waic = TRUE),
                   control.inla = list(strategy = "adaptive", int.strategy = "eb"),
                   verbose = FALSE)

# ---------------------------------------------------
# EXTRAER PREDICCIONES
# ---------------------------------------------------
alpha.mean <- model.pred$summary.random$alpha$mean

# Alpha:
alpha.pred <- as.vector(A.alpha.pred %*% alpha.mean)
alpha.pred.mat <- matrix(alpha.pred, nrow = dims, ncol = dims)

# rw2:
rw2 <- model.pred$summary.random$X_rw2
rw2_effect <- rw2$mean[match(pred$X_rw2, rw2$ID)]

# Respuesta:
beta0_hat.pred <- model.pred$summary.fixed["intercept", "mean"]
y_hat <- beta0_hat.pred + rw2_effect + alpha.pred
y_hat.mat <- matrix(y_hat, nrow = dims, ncol = dims)




# ------------------------------------------------------------------------------
# ------------------------ GRAFICA RW2 CON INTERVALOS --------------------------
# ------------------------------------------------------------------------------
# Extraer media e intervalos del RW2 para cada punto de predicción
rw2_effect_mean <- rw2$mean[match(pred$X_rw2, rw2$ID)]
rw2_effect_lwr  <- rw2$`0.025quant`[match(pred$X_rw2, rw2$ID)]
rw2_effect_upr  <- rw2$`0.975quant`[match(pred$X_rw2, rw2$ID)]

df_rw2 <- data.frame(
  X = as.vector(X_matrix),
  eta_real = as.vector(eta.real_matrix),
  eta_rw2 = rw2_effect_mean,
  rw2_lwr = rw2_effect_lwr,
  rw2_upr = rw2_effect_upr
)

# Ordenar como antes
df_rw2 <- df_rw2[order(df_rw2$X), ]

gg_rw2 <- ggplot(df_rw2, aes(x = X)) +
  
  geom_point(
    aes(y = eta_real),
    color = "#2C7FB8",
    alpha = 0.3,
    size = 0.3
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  geom_ribbon(
    aes(ymin = rw2_lwr, ymax = rw2_upr),
    fill = "darkred",
    alpha = 0.25
  ) +
  
  geom_line(
    aes(y = eta_rw2),
    color = "darkred",
    linewidth = 1.2
  ) +
  
  labs(
    title = expression(
      paste(
        "Estimación RW2 del efecto ",
        eta(s) == beta(s) %.% X(s)
      )
    ),
    x = expression(X(s)),
    y = "Efecto parcial"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    panel.grid = element_blank()
  )

gg_rw2
