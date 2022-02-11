library(stats)

# Part B
U <- runif(100000, 0, 1)
Cauchy <- 2 + tan(pi * (U - 0.5))

# Part C
sample_mean <- mean(Cauchy)
sample_sd <- sd(Cauchy)
sample_quantiles <- quantile(Cauchy, c(0.01, 0.5, 0.99))
print(sample_mean)
print(sample_sd)
print(sample_quantiles)

# Part D
Q <- function(p) {
  tan(pi * (p - 0.5)) + 2
}
print(Q(c(0.01, 0.5, 0.99)))
print((Q(c(0.01, 0.5, 0.99)) - sample_quantiles)/sample_quantiles)

# Part E
B <- 10000
n <- c(10, 100, 1000, 10000)
mu <- 2
epsilon <- matrix(0, B, 4)
eta <- matrix(0, B, 4)

for (i in 1:B) {
  for (j in 1:length(n)){
    U <- runif(n[j], 0, 1)
    Cauchy <- mu + tan(pi * (U - 0.5))
    
    Y_bar = (1 / n[j]) * sum(Cauchy)
    epsilon[i, j] = Y_bar - mu
    eta[i, j] = quantile(Cauchy, 0.5) - mu
  }
}

for (i in 1:length(n)) {
  print(paste(n[i], " Cauchy(2,1) variables:", sep=""))
  
  print("Epsilon quantiles")
  print(quantile(epsilon[,i], c(0.05, 0.5, 0.95)))
  
  print("Eta quantiles")
  print(quantile(eta[,i], c(0.05, 0.5, 0.95)))
}





