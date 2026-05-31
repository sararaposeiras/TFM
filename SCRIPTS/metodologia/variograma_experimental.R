
# -----------------------------------
# Variograma experimental
# Inspirada en Zuur et al. (2017)
# -----------------------------------
library(ggplot2)
library(MASS)
set.seed(1423)
n_sp = 20
# Distancias:
d <- seq(0, 10, length.out = n_sp)

# Variograma con dependencia espacial:
# pequeño a distancias cortas, aumenta y se estabiliza
vario_dep <- 1 - exp(-d / 1.8) + rnorm(n_sp, 0, 0.03)

# Variograma sin dependencia espacial:
# aproximadamente plano
vario_ind <- rep(1, n_sp) + rnorm(n_sp, 0, 0.03)

df <- rbind(
  data.frame(distancia = d, semivarianza = vario_dep,
             tipo = "Con dependencia espacial"),
  data.frame(distancia = d, semivarianza = vario_ind,
             tipo = "Sin dependencia espacial")
)

ggplot(df, aes(x = distancia, y = semivarianza)) +
  geom_point(size = 2) +
  facet_wrap(~ tipo, ncol = 2) +
  labs(x = "Distancia", y = "Semivarianza") +
  ylim(0, 1.15) +
  theme_bw(base_size = 14)

