#genera 10 números al azar entre 0 y 1
runif(10)

#genero 10 exponenciales con parámetro lambda = 2
rexp(10, 2)

#hago lo mismo con 10000 exponenciales y busco la esperanza
x <- rexp(10000, 2)
mean(x)

#genero 10000 números aleatorios entre 0 y 1
u <- runif(10000)

#cuando está entre 0 y 0.5 toma valor 1, entre 0.5 y 0.8 2 y así
x <- 1 * (u >= 0 & u < 0.5) + 2 * (u >= 0.5 & u < 0.8) + 5 * (u >= 0.8 & u < 1)

#muestra los promedios esperando tener algo parecido a 0.5 0.3 0.2
c(mean(x==1), mean(x==2), mean(x==5))
