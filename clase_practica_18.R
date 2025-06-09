#a)
#generamos 1000 experimentos aleatorios U(0,1)
X1 <- runif(1000, min = 0, max = 1)

#armo el histograma de 30 rectángulos
hist(X1, breaks = 30, prob = TRUE)# este último para que esté bien la escala


#b)
#armo una matriz para poner 1000 veces 2 experimentos Xi ~ U(0,1)
matriz2 <- matrix(data = 0, nrow = 2, ncol = 1000)

#lleno las matriz con los números
for(i in 1:1000){
  matriz2[,i] <- runif(2)
}

#armo un vector X con los promedios de cada dupla de la matriz
X <- colMeans(matriz2)

#grafico
hist(X, prob = TRUE)
#le superpongo una normal con una media como la de X
curve(dnorm(x,mean(X),sd(X)),add = TRUE)


#c)
matriz5 <- matrix(data = 0, nrow = 5, ncol = 1000)


#d)
matriz30 <- matrix(0, nrow = 30, )


#Ejercicio mejor hecho:


## 1

# a
experimentos_a <- runif(1000)
hist(runif(1000))
# Parece la densidad de una uniforme 0 1

# b
experimentos_b <- replicate(1000, mean(runif(2)))
hist(experimentos, col = "red")
# Parece una normal/binomial

# c
experimentos_c <- replicate(1000, mean(runif(5)))
hist(experimentos_c, add = TRUE, col = "green")
# Es mÃ¡s parecido a una normal

# d
experimentos_d <- replicate(1000, mean(runif(30)))
hist(experimentos_d, add = TRUE, col = "blue")
# MÃ¡s datos caen cerca de la media
?hist

# e
experimentos_e <- replicate(1000, mean(runif(500)))
hist(experimentos_e, add = TRUE, col = "yellow")

# f
experimentos_f <- replicate(1000, mean(runif(1200)))
hist(experimentos_f, add = TRUE, col = "purple")

table <- cbind(experimentos_a,
               experimentos_b,
               experimentos_c,
               experimentos_d,
               experimentos_e,
               experimentos_f)

boxplot(table)

qqnorm(experimentos_a)
qqnorm(experimentos_b)
qqnorm(experimentos_c)
qqnorm(experimentos_d)
qqnorm(experimentos_e)
qqnorm(experimentos_f)

# g
estandarizados <- matrix(nrow = 1000, ncol = 6)
ns <- c(1, 2, 5, 30, 500, 1200)
for(i in 1:6) {
  estandarizados[,i] <- (table[,i] - .5)/sqrt(1/(12*ns[i]))
}
boxplot(estandarizados)

# h
cauchys <- matrix(nrow = 1000, ncol = 6)
for(i in 1:6) {
  cauchys[,i] <- replicate(1000, mean(rcauchy(ns[i])))
}

boxplot(cauchys)
