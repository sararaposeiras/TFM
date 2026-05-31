
# ------------------------------------------------
# Diagrama de burbujas espacial de residuos
# Inspirada en Zuur et al. (2017)
# ------------------------------------------------
library(ggplot2)
library(MASS)
set.seed(1423)
n_sp <- 50 # numero observaciones

# Coordenadas espaciales (malla)
x <- runif(n_sp, 0, 10)
y <- runif(n_sp, 0, 10)
coords <- cbind(x, y)

# Distancias
dist_mat <- as.matrix(dist(coords))

# Matriz de covarianzas
cov_mat <- exp(-(dist_mat^2) / 2)

# Residuos
res_sp <- as.numeric(mvrnorm(1, mu = rep(0, n_sp), Sigma = cov_mat))

# Representacion gráfica bubbleplot
df_sp <- data.frame(
  x = x,
  y = y,
  residuo = res_sp,
  signo = ifelse(res_sp >= 0, "Positivo", "Negativo")
)

ggplot(df_sp, aes(x = x, y = y)) +
  geom_point(
    aes(size = abs(residuo), fill = signo),
    shape = 21,
    color = "black"
  ) +
  scale_fill_manual(values = c("Negativo" = "white", "Positivo" = "black")) +
  scale_size(range = c(1, 7)) +
  coord_equal() +
  labs(x = "Coordenada X", y = "Coordenada Y",
       size = "|Residuo|", fill = "Signo") +
  theme_bw()
