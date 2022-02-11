library(stats)

# Define parameters of the simulation 
B = 10000
N = c(50,200,1000)
V = c(2.5,5.5,26)
thetas = c(-3.424,-2.696, -2.428)
c = -2
# 
# theta_hats = array(rep(0,B*3*3), dim=c(B,3,3))
# 
# # Perform simulation
# for (v in 1:3) {
#   beta = sqrt((V[v] - 2)/V[v])
#   for (n in 1:3) {
#     for (b in 1:B) {
#       U = rt(N[n],V[v])
#       Y = beta * U
#       
#       ind = sum(Y < c)
#       
#       if (ind == 0) {
#         theta_hats[b,n,v] = c
#       } else {
#         theta_hats[b,n,v] = (sum(Y[Y < c]))/ind
#       }
#     }
#   }
# }
# 
# # Calculate MSE values
# MSE = matrix(0, 3, 3)
# for (n in 1:3) {
#   for (v in 1:3) {
#     MSE[n,v] = (1/B) * sum((theta_hats[,n,v]-thetas[v])**2)
#   }
# }
# 
# print("Table of MSE values")
# print(MSE)
# 
# # Calculate a values
# a = matrix(0,3,3)
# for (n in 1:3) {
#   for (v in 1:3) {
#     a[n,v] = N[n] * MSE[n,v]
#   }
# }
# 
# print("Table of a(v) values")
# print(a)

# Approximate a(v) values
print("Approximate a(v) values:")

a_50k = matrix(0,B,3)

for (v in 1:3) {
  beta = sqrt((V[v] - 2)/V[v])
    for (b in 1:B) {
      U = rt(50000,V[v])
      Y = beta * U

      ind = sum(Y < c)

      if (ind == 0) {
        a_50k[b,v] = c
      } else {
        a_50k[b,v] = (sum(Y[Y < c]))/ind
      }
    }
}

MSE = c(0,0,0)
for (v in 1:3) {
  MSE[v] = (1/B) * sum((a_50k[,v]-thetas[v])**2)
}

a = c(0,0,0)

for (v in 1:3) {
  a[v] = 50000 * MSE[v]
}

print(paste("Approximate value of a(2.5)",a[1]))
print(paste("Approximate value of a(5.5)",a[2]))
print(paste("Approximate value of a(26)",a[3]))

# ~ 600, ~ 26, ~ 7

