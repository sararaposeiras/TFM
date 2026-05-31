
# ------------------------------------------------------------
# Grafica para ilustrar el efecto de una prior difusa
# vs informativa
# Inspirada en Zuur et al. (2017)
# ------------------------------------------------------------

# Generamos datos:
theta <- seq(-10, 10, length.out = 1000)


# Verosimilitud:
veros <- dnorm(theta, mean = -3, sd = 2)


# Prior difusa:
prior_difusa <- dnorm(theta, mean = 0, sd = 15)

# Distribucion a posteriori (prior difusa)
post_difusa <- veros * prior_difusa
post_difusa <- post_difusa / max(post_difusa) * max(veros)


# Prior informativa:
prior_info <- dnorm(theta, mean = 7, sd = 0.5)

# Distribucion a posteriori (prior informativa)
post_info <- veros * prior_info
post_info <- post_info / max(post_info) * max(veros)


# Gráficamente:
par(mfrow = c(2, 3))
# Fila 1: priori difusa
plot(theta, veros, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Verosimilitud")
abline(v = 0, lty = 2)

plot(theta, prior_difusa, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Prior difusa")
abline(v = 0, lty = 2)

plot(theta, post_difusa, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Posterior")
abline(v = 0, lty = 2)

# Fila 2: priori informativa
plot(theta, veros, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Verosimilitud")
abline(v = 0, lty = 2)

plot(theta, prior_info, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Prior informativa")
abline(v = 0, lty = 2)

plot(theta, post_info, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = "Densidad",
     main = "Posterior")
abline(v = 0, lty = 2)
par(mfrow = c(1, 1))
