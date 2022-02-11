library(stats)

# Read data
Z = read.csv("jesse.csv")
X = Z$middle.school
Y = Z$high.school

# Define function
mu_circ <- function(x) {
  band <- Z[Z$middle.school >= x - 10 & Z$middle.school <= x + 10, ]
  improvements <- band$middle.school - band$high.school
  
  return(sum(improvements) / nrow(band))
}

# Plot mu_circ against x
x <- 64:124
plot(x, sapply(x, FUN=mu_circ), 
       xlab="Initial Time, in seconds", 
       ylab="Average Improvement, in seconds", col="red")

# Define second version of function
mu_circ_med <- function(x) {
  band <- Z[Z$middle.school >= x - 10 & Z$middle.school <= x + 10, ]
  improvements <- band$middle.school - band$high.school
  return(median(improvements))
}

# Plot again
plot(x, sapply(x, FUN=mu_circ_med),
     xlab="Initial Time, in seconds",
     ylab="Median Improvement, in seconds", col="blue")



