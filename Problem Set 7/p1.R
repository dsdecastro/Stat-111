library(stats)

jill <- read.csv("jill.csv")
n <- length(jill$Y)

mu <- c(12400:12700/100)
mu_0 <- 126
B <- 500

power <- c()
for (m in mu) {
  reject <- c()
  for (i in 1:B) {
    U <- rnorm(n)
    Y <- m * (1 + exp(-4 + 0.5 * U)) / (1 + exp(-4))
    mu_hat <- median(Y)
    
    dx <- density(Y)
    fden <- approx(dx$x, dx$y, xout=m)$y
    
    SE <- sqrt(1 / (4 * n * fden ** 2))
    
    lower <- mu_hat - 1.96 * SE
    upper <- mu_hat + 1.96 * SE
    
    reject <- append(reject, as.integer((mu_0 < lower)|(mu_0 > upper)))
  }
  power <- append(power, mean(reject))
}

png(file="1e.png", width=6, height=4, units="in", res=100)
plot(mu, power)
abline(h = 0.5, col="blue", lty=2)
legend("bottomleft", legend=c("y = 0.5"), col=c("blue"), lty=c(2))
dev.off()

for (i in 1:length(power)) {
  if (power[i] < 0.5) {
    print(i) 
    print(mu[i]) 
    print(power[i])
    break;
  }
}

for (i in 170:length(power)) {
  if (power[i] > 0.5) {
    print(i) 
    print(mu[i]) 
    print(power[i])
    break;
  }
}

d <- 0.4
c <- 2
mu_x <- function(x) {
  return(d * x + c * (x ^ 2 - 2))
}
theta <- function(x) { return(0.4 * x) }
x <- c(-300:300/100)

png(file="4c.png", width=6, height=4, units="in", res=100)
plot(x, mu_x(x), ylab="", type="l", col="black")
lines(x, theta(x), col="blue", lty=2, ylab="")
legend("top", legend=c("mu(x)", "theta * x"), col=c("black", "blue"), lty=c(1,2))
dev.off()
