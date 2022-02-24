# Problem 2
library(stats)

# (b)
sigma = 1
theta = 0

f_Y <- function(y, v) {
  return(gamma((v+1)/2) / (sqrt(v * pi * sigma) * gamma(v/2)) * (1 + (y - theta)**2/(sigma**2 * v))**(-(v+1)/2))
}

Eff <- function(v) {
  return(4 * f_Y(theta, v)**2 * (sigma**2 * v)/(v-2))
}

v = seq(1,10,by=0.05)

plot(v, Eff(v), type="l", col="blue")

# (c)
abline(h=1, col="red", lab="y = 1")

rang = seq(4.6,4.7,by=0.001)
for (i in rang) {
  if (Eff(i) <= 1) {
    print(i)
    break
  }
}