
# --------------------------------------------------------------------
# Figuras para ilustrar la función de los parámetros de la función
# de correlación de matern
# --------------------------------------------------------------------

library(fields)
library(MASS)

set.seed(1423)

# Rejilla de puntos
n <- 100 
x <- seq(0, 10, length.out = n)
y <- seq(0, 10, length.out = n)

# Coordenadas:
coords <- expand.grid(x = x, y = y)

# Matriz de distancias:
D <- rdist(coords)


# Función para simulación del campo espacial:
simular_matern <- function(rango, suavidad){
  Sigma <- Matern(D,
                  range = rango,
                  smoothness = suavidad)
  z <- mvrnorm(
    n = 1,
    mu = rep(0, nrow(coords)),
    Sigma = Sigma
  )
  matrix(z, n, n)
}


# Simulaciones:
z1 <- simular_matern(rango = 0.5, suavidad = 0.5)

z2 <- simular_matern(rango = 3, suavidad = 0.5)

z3 <- simular_matern(rango = 0.5, suavidad = 3)

z4 <- simular_matern(rango = 3, suavidad = 3)



# Representación gráfica:
par(mfrow = c(2,2))
image.plot(
  x, y, z1,
  main = "Rango pequeño\nSuavidad baja",
  axes = FALSE
)

image.plot(
  x, y, z2,
  main = "Rango grande\nSuavidad baja",
  axes = FALSE
)

image.plot(
  x, y, z3,
  main = "Rango pequeño\nSuavidad alta",
  axes = FALSE
)

image.plot(
  x, y, z4,
  main = "Rango grande\nSuavidad alta",
  axes = FALSE
)
par(mfrow = c(1,1))
