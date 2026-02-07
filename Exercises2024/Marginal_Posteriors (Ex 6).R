# Pakete laden
library(tidyverse)
library(MASS)
library(plotly)

# MCMC Samples simulieren (bimodale Posterior)
set.seed(42)
mu1 <- c(-1.5, -1)
mu2 <- c(2, 2)
Sigma <- matrix(c(1, 0.6, 0.6, 1), 2)
samples <- rbind(mvrnorm(4000, mu1, Sigma), mvrnorm(6000, mu2, Sigma))
mcmc_samples <- as.data.frame(samples)
colnames(mcmc_samples) <- c("theta1", "theta2")

# 2D Dichte schätzen
kde <- with(mcmc_samples, kde2d(theta1, theta2, n = 100))

# Marginale Dichten berechnen
dens_theta1 <- density(mcmc_samples$theta1, n = 100)
dens_theta2 <- density(mcmc_samples$theta2, n = 100)

# Z für 2D posterior
z_surface <- kde$z
x_vals <- kde$x
y_vals <- kde$y

# 3D Plot mit Marginalprojektionen
plot_3d <- plot_ly()

# Hauptposteriorfläche
plot_3d <- plot_3d %>%
  add_surface(x = ~x_vals, y = ~y_vals, z = ~z_surface,
              showscale = FALSE,
              colorscale = "Viridis",
              name = "Posterior")

# Projektion der marginalen p(theta1) auf die θ₁-Z-Ebene (bei min(y))
plot_3d <- plot_3d %>%
  add_trace(x = ~dens_theta1$x,
            y = rep(min(y_vals), length(dens_theta1$x)),
            z = ~dens_theta1$y,
            type = 'scatter3d',
            mode = 'lines',
            line = list(color = 'blue'),
            name = "p(θ₁)")

# Projektion der marginalen p(theta2) auf die θ₂-Z-Ebene (bei min(x))
plot_3d <- plot_3d %>%
  add_trace(x = rep(min(x_vals), length(dens_theta2$x)),
            y = ~dens_theta2$x,
            z = ~dens_theta2$y,
            type = 'scatter3d',
            mode = 'lines',
            line = list(color = 'red'),
            name = "p(θ₂)")

# Achsen und Layout
plot_3d <- plot_3d %>%
  layout(
    title = "3D Posterior mit marginalen Dichten",
    scene = list(
      xaxis = list(title = "θ₁"),
      yaxis = list(title = "θ₂"),
      zaxis = list(title = "Posterior-Dichte"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))
    )
  )

plot_3d
