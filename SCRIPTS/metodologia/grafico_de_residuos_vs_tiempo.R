

# -------------------------------------
# Gráfico residuos vs. tiempo
# Inspirada en Zuur et al. (2017)
# -------------------------------------
library(ggplot2)
library(MASS)
set.seed(1423)
n <- 100 # longitud serie de tiempo
tiempo <- 1:n

# Residuos con dependencia temporal: arima
res_dep <- as.numeric(arima.sim(model = list(ar = 0.8), n = n))

# Residuos sin dependencia temporal: distribucion gaussiana
res_ind <- rnorm(n)

# Representacion grafica residuo vs. tiempo:
df_tiempo <- rbind(
  data.frame(tiempo = tiempo, residuo = res_dep, tipo = "Con dependencia temporal"),
  data.frame(tiempo = tiempo, residuo = res_ind, tipo = "Sin dependencia temporal")
)

ggplot(df_tiempo, aes(x = tiempo, y = residuo)) +
  geom_point(size = 1.5) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ tipo, ncol = 2) +
  labs(x = "Tiempo", y = "Residuos") +
  theme_bw()
