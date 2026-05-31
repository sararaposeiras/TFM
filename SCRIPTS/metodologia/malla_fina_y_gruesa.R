
# --------------------------------------------------------------------
# Grafica para ilustrar la diferencia entre una malla 
# fina y una gruesa
# --------------------------------------------------------------------

# Librerias necesarias
library(INLA)

# Semilla
set.seed(1423)

# Coordenadas
dims <- 101#dimensiones
n <- dims^2
grid_list <- list(x = seq(0, 10, length = dims),
                  y = seq(0, 10, length = dims))
lon <- rep(grid_list$x, each = dims)
lat <- rep(grid_list$y, times = dims)

# Muestreo de puntos
n_samples <- 50
sample_aleatorio <- sample(1:n, n_samples, replace = FALSE)
Loc <- matrix(c(lon[sample_aleatorio], lat[sample_aleatorio]), ncol=2)

par(mfrow = c(1,2))
# Malla fina:
mesh <- inla.mesh.2d(Loc, max.edge = 0.5, cutoff = 0.5)
plot(mesh)
title(main = "Malla fina")
points(Loc, col = "blue", pch = 16, cex=1)
mesh$n # numero de vertices

# Malla gruesa:
mesh <- inla.mesh.2d(Loc, max.edge = 3.5, cutoff = 0.5)
plot(mesh)
title(main = "Malla gruesa")
points(Loc, col = "blue", pch = 16, cex=1)
mesh$n # numero de vertices
par(mfrow = c(1,1))


