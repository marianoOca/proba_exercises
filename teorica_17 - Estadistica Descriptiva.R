d <- c(3, 6, 77, 84, 11, 45, 56, 12, 30, 98, 11, 99, 3)
d2 <- c(1, 3,4, 5, 7,8, 956, 23, 763, 3,6, 453634, 1)

#media de un conjunto de datos
mean(d)

#mediana de un conjunto de datos
median(d)

#muestra los cuantiles
quantile(d)

#media podada, quitando el 10% de cada extremo
mean(d, 0.1)

#varianza dos formas iguales V(X) = E(X^2) - E(X)^2
mean(d^2)-mean(d)^2

mean((d-mean(d))^2)

var(d)

#distancia intercuartil
IQR(d)

#Mediana de Desviaciones Absolutas
mad(d, constant = 1)

#graficamos el histograma
hist(d, probability = TRUE)

#graficamos boxplot
boxplot(d)

#correlación entre d y d2
cor(d, d2)

#linear model: estimadores de mínimos cuadrados
lm( )
