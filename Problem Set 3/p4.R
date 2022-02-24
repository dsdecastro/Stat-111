# Problem 4
library(stats)

# (a)
videogame64 <- read.csv("videogame64.csv")
n = nrow(videogame64)
M <- ceiling(2.289 * n**(1/3))

f_hat <- function(y) {
  return(M / n * sum(videogame64$Y > floor(M * y) / M & videogame64$Y <= ceiling(M * y) / M))
}

v <- seq(0, 1, by=0.001)
out <- rep(0, length(v))
for (i in 1:length(v)) {
  out[i] = f_hat(v[i])
}

plot(v, out, type="l", col="darkgreen", xlab="y", ylab="f_hat(y)", main="videogame64: f_hat(y) vs. y")

# (b)
videogame512 <- read.csv("videogame512.csv")
n = nrow(videogame512)
M <- ceiling(2.289 * n**(1/3))

f_hat <- function(y) {
  return(M / n * sum(videogame512$Y > floor(M * y) / M & videogame512$Y <= ceiling(M * y) / M))
}

out <- rep(0, length(v))
for (i in 1:length(v)) {
  out[i] = f_hat(v[i])
}

plot(v, out, type="l", col="navy", xlab="y", ylab="f_hat(y)", main="videogame512: f_hat(y) vs. y")


