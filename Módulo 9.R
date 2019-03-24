xn <- c(1,1,0,0) # Sequência de entrada

N <- 4 # número de pontos da sequência

k <- seq(from=0, to = N-1, by = 1)

n <- seq(from=0, to = N-1, by = 1)

# observe que na função abaixo, -1i = número imaginário --> 0 - 1i
W <- function(k, n, N)
{
  exp(-1i * (2 * pi / N) * k *n)
}


X <- complex(length.out = N)
print(X, digits = 2)

X[1] <- sum(W(k[1], n, N) * xn)
X[2] <- sum(W(k[2], n, N) * xn)
X[3] <- sum(W(k[3], n, N) * xn)
X[4] <- sum(W(k[4], n, N) * xn)

print(X, digits = 2)


#Exercício 1
xn <- c(-1,1,2,-2) # Sequência de entrada

N <- 4 # número de pontos da sequência

k <- seq(from=0, to = N-1, by = 1)

n <- seq(from=0, to = N-1, by = 1)

# observe que na função abaixo, -1i = número imaginário --> 0 - 1i
W <- function(k, n, N)
{
  exp(-1i * (2 * pi / N) * k *n)
}


X <- complex(length.out = N)
print(X, digits = 2)

X[1] <- sum(W(k[1], n, N) * xn)
X[2] <- sum(W(k[2], n, N) * xn)
X[3] <- sum(W(k[3], n, N) * xn)
X[4] <- sum(W(k[4], n, N) * xn)

print(X, digits = 2)


#Transformada inversa
X <- c(0+0i,-3-3i,2+0i,-3+3i) # Sequência de entrada

N <- 4 # número de pontos da sequência

k <- seq(from=0, to = N-1, by = 1)

n <- seq(from=0, to = N-1, by = 1)

# observe que na função abaixo, -1i = número imaginário --> 0 - 1i
W <- function(k, n, N)
{
  exp(1i * (2 * pi / N) * k *n)
}


x <- complex(length.out = N)
print(x, digits = 2)

x[1] <- (1/N)*sum(W(k[1], n, N) * X)
x[2] <- (1/N)*sum(W(k[2], n, N) * X)
x[3] <- (1/N)*sum(W(k[3], n, N) * X)
x[4] <- (1/N)*sum(W(k[4], n, N) * X)

print(x, digits = 2)



#Transformada por matriz

W = matrix(
  
  c(1, 1, 1, 1,
    1, exp(-1i * (2 * pi / 4) * 1), exp(-1i * (2 * pi / 4) * 2), exp(-1i * (2 * pi / 4) * 3),
    1, exp(-1i * (2 * pi / 4) * 2), exp(-1i * (2 * pi / 4) * 4), exp(-1i * (2 * pi / 4) * 6),
    1, exp(-1i * (2 * pi / 4) * 3), exp(-1i * (2 * pi / 4) * 6), exp(-1i * (2 * pi / 4) * 9)),
  nrow=4,
  ncol=4, 
  byrow = TRUE)

x = matrix(
  
  c(-1, 1, 2, -2),
  nrow=4,
  ncol=1, 
  byrow = TRUE)

 X = W %*% x #multiplicação matricial


 #Transformada inversa por matriz
 
 Wi = matrix(
   
   c(1, 1, 1, 1,
     1, exp(-1i * (2 * pi / 4) * (-1)), exp(-1i * (2 * pi / 4) * (-2)), exp(-1i * (2 * pi / 4) * (-3)),
     1, exp(-1i * (2 * pi / 4) * (-2)), exp(-1i * (2 * pi / 4) * (-4)), exp(-1i * (2 * pi / 4) * (-6)),
     1, exp(-1i * (2 * pi / 4) * (-3)), exp(-1i * (2 * pi / 4) * (-6)), exp(-1i * (2 * pi / 4) * (-9))),
   nrow=4,
   ncol=4, 
   byrow = TRUE)
 
 Xi = matrix(
   
   c(0+0i, -3-3i,  2+0i, -3+3i),
   nrow=4,
   ncol=1, 
   byrow = TRUE)
 
 xi = (1/4) * Wi %*% Xi #multiplicação matricial

#inv = 1/4*conj(W)
#inv1 = inv %*% X
 
X <- c(-1, 1, 2, -2) # Sequência de entrada
N <- 4 # número de pontos da sequência
fs <- 33
 
delta = fs/N

fk <- c((-1)*delta, (1)*delta, (2)*delta, (-2)*delta)





