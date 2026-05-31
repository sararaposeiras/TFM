
# ----------------------------------------------------
# Grafica función de autocorrelacion de los residuos
# Inspirada en Zuur et al. (2017)
# ----------------------------------------------------
library(ggplot2)
library(MASS)
set.seed(1423)
n <- 100 # longitud serie de tiempo
tiempo <- 1:n

# Residuos con dependencia temporal: arima
res_dep <- as.numeric(arima.sim(model = list(ar = 0.8), n = n))

# Residuos sin dependencia temporal: distribucion gaussiana
res_ind <- rnorm(n)

par(mfrow = c(1, 2))

# Funcion de autocorrelacion con dependencia temporal:
acf(res_dep, main = "Con dependencia temporal")

# Funcion de autocorrelacion sin dependencia temporal:
acf(res_ind, main = "Sin dependencia temporal")

par(mfrow = c(1, 1))
