setwd("C:/Users/maria/Desktop/Taller/Proba")
datos<-read.table("datos_coronavirus.txt", header = TRUE, check.names = FALSE)
datos
View(datos)
head(datos)
tail(datos)
colnames(datos)
rownames(datos)
datos[c(15,50)]
datos[1:3,]
datos[,4]
datos$`2/19/20`
ncol(datos)
datos[,82]

#cuantos paises tienen más de 5000 casos?
datos[,ncol(datos)]>5000
sum(datos[,ncol(datos)]>5000)

#qué paises tienen más de 150000 casos?
paisesBananas <- which(datos[,ncol(datos)]>150000)
rownames(datos)[paisesBananas]
rownames(datos)[c(78,139,157)]

#cuántos casos hay en el mundo al día de hoy?
sum(datos[,ncol(datos)])

#graficamos los casos en argentina:
i_arg <- which(rownames(datos)=="Argentina")
n_dias <- ncol(datos)
casos_argentina <- datos[i_arg,]
plot(1:n_dias, casos_argentina)

#graficamos desde que empezaron los contagios
colnames(datos)
casos_argentina <- datos[i_arg, 42:68]
plot(42:68, casos_argentina)

#hacemos lo mismo pero en escala logarítmica
plot(42:68, log(casos_argentina))

#graficamos los casos en el mundo
x <- 1:ncol(datos)
y <- colSums(datos)
plot(x,y, type = "l")       #escala lineal
plot(x,log(y), type = "l")  #escala logarítmica