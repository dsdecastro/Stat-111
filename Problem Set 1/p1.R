library(stats)
library(ggplot2)

# Read csv file
a = read.csv("jesse.csv")

# Isolate high school times
Y = a$high.school

# Find the minimum and maximum times
print(c(min(Y), max(Y)))

# Create histogram
hist(Y, main="High School 400m Times", 
     xlab="Time, in seconds", xlim=c(45, 175), ylab="Frequency",
     col="red2")

# Compute and Graph ECDF
Fn <- ecdf(Y)
ggplot(a, aes(high.school)) + stat_ecdf(geom="step") +
  labs(title="ECDF of High School 400m Times", x="Time, in seconds", y="F")+
  theme_light()

# Compute Summary Statistics
y_bar = mean(Y)
s_y = sd(Y)
quantiles = quantile(Y, c(0.01, 0.5, 0.99))
print(y_bar)
print(s_y)
print(quantiles)
print(quantiles[2] - quantiles[1])
print(quantiles[3] - quantiles[2])

# Part C
print(0.01 * length(Y))

# Part D
q_tilde <- function(p) {
  y_bar + s_y * qnorm(p)
}
print(q_tilde(c(0.01, 0.5, 0.99)))