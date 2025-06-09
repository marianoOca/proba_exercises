##1##

  setwd("C:/Users/maria/Desktop/Taller/Proba/")
  
  alfajores <- read.table("DatosP6/alfajores.txt", header = TRUE)
  View(alfajores)
  
  # fabrica==0 -> Quilmes
  # fabrica==1 -> Pilar
  
  #a = prob. de que la caja sea de Quilmes
  mean(alfajores$fabrica==0)
  
  #b = prob. sean 3 decetuosas de Quilmes
  mean(alfajores$defectuosos==3 & alfajores$fabrica==0)
  
  #c = prob. sea x defectuosas y de Quilmes(0) o Pilar(1)
  probPuntual = function(x,y){
    mean(alfajores$defectuosos==x & alfajores$fabrica==y)
  }
  
  #chequeo con la prob de antes
  probPuntual(3,0)
  
  #d
  # columna 1, todas las filas, el promedio, esto me da la esperanza
  esperanza <- mean(alfajores[,1])
  esperanza
  
  # calculamos la E(x^2)
  puntualx = function(x) {
    ( sum ((alfajores[,1] == x) & (alfajores[,2] = 0))
      +
      sum ((alfajores[,1] == x) & (alfajores[,2] = 1)) ) /nrow(alfajores)
  }
  esperanza2=0
  for(i in 0:4){
    esperanza2 = esperanza2 + puntualx(i)*(i^2)
  }
  
  # v(x) = E(x^2) - E(x)^2
  varianza <- esperanza2 - esperanza^2
  varianza
  
  #e = con 3 defectuosos de Quilmes / cant de quilmes
  sum(alfajores$defectuosos==3 & alfajores$fabrica==0)/sum(alfajores$fabrica==0)
  
  #f = cajas con 3 defectuosos / todas las cajas
  sum(alfajores$defectuosos==3) / nrow(alfajores)
  # o
  mean(alfajores$defectuosos==3)
  
  #g = con 3 defectuosos de Quilmes / cant de alfajores con 3 defectuosos
  sum(alfajores$defectuosos==3 & alfajores$fabrica==0) / sum(alfajores$defectuosos==3)
  
  #h = con 3 defectuosos de Pilar / cant de alfajores con 3 defectuosos
  sum(alfajores$defectuosos==3 & alfajores$fabrica==1) / sum(alfajores$defectuosos==3)


##2##

  #Header = FAlse porque no tengo nombre de Columna
  lamps <- read.table("DatosP6/lamparas.txt", header = FALSE)
  View(lamps)
  
  #a  cuantas lámparas > 30 / cant de lamparas
  mean(lamps[,1]>30) # lamps$V1 = lamps[,1]
  #o
  sum(lamps[,1]>30)/nrow(lamps)
  
  #b
  acumulada <- function(t){
    mean(lamps[,1]<=t)
  }
  
  #pruebo que esto sea igua a lo anterior
  1 - acumulada(30)
  
  #probabilidades puntuales por defecto
  plot(lamps[,1])
  #con esto calculo la acumulada
  plot(ecdf(lamps[,1]))
  
  #c
  #El 90 % de las lámparas producidas por esta fábrica dura más de
  quantile(lamps[,1], 0.9)
  #horas y el 10 % dura menos de
  quantile(lamps[,1], 0.1)
  #horas.


##3##

  #así lo guardamos como vector en vez de tabla
  prom <- scan("DatosP6/graduados.txt")
  prom
  
  #a
  #media muestral = promedio
  mean(prom)
  
  #mediana muestral
  quantile(prom, 0.5)
  # o
  median(prom)
  
  #b
  #desvio standar
  sd(prom)
  
  #distancia intercuartil
  IQR(prom)
  
  #c
  hist(prom, probability = TRUE)
  curve(dnorm(x, mean(prom), sd(prom)),col = "blue", lwd = 2, add = TRUE)
  
  #f
  density(prom)
  lines(density(prom))
  
  #d
  boxplot(prom)
  
  #e = tiene distribución normal


##4##
  ciudades <- read.table("DatosP6/ciudades.txt", header = TRUE)
  View(ciudades)
  boxplot(ciudades)
  
  #b
  var(ciudades$Argentina)
  var(ciudades$EEUU)
  var(ciudades$Holanda)
  var(ciudades$Japon)
  
  #La mas homogenea es holanda


##5##

  ingresos <- scan("DatosP6/ingresos.txt")
  ingresos
  
  #a
  #busco el mínimo
  min(ingresos)
  
  #hago cant ingresos = mínimo / cant total de ingresos
  mean(ingresos==min(ingresos))
  
  #b
  quantile(ingresos, 0.9)
  
  #c
  #media muestral
  mean(ingresos)
  
  #mediana muestral
  median(ingresos)
  
  #mediana podada .10 de cada lado
  median(ingresos, trim = .1)
  
  #d
  #desvio standar
  sd(ingresos)
  
  #distancia intercuartil
  IQR(ingresos)
  
  #e
  hist(ingresos)
  boxplot(ingresos)


##6##

  #a
  data_norm_25 = rnorm(25)
  data_norm_100 = rnorm(100)
  
  
  #cantidad de muestreos que quiero hacer
  n = 10;
  
  data_norm_n = rnorm(n)
  
  hist(data_norm_n)
  
  #esto mide la relación entre una normal ideal y la muestra
  qqnorm(data_norm_n, pch = 1, frame = FALSE)
  
  #esto me trasa una línea para visualizar mejor la relación
  qqline(data_norm_n, col = "steelblue", lwd = 2)
  
  #relación entre 2 muestras
  qqplot(data_norm_25,data_norm_100 )
  
  #b
  n = 25;
  m = 100;
  
  data_gamma_n = rgamma(n, 5, 1/2)
  data_gamma_m = rgamma(m, 5, 1/2)
  hist(data_gamma_n)
  hist(data_gamma_m)
  qqplot(data_gamma_n,data_gamma_m)
  abline(a = 0, b = 1, col = "steelblue")
  
  #c
  n = 25;
  m = 1000;
  
  data_n = rnorm(n)/ runif(n)
  data_m = rnorm(m)/ runif(m)
  hist(data_n)
  hist(data_m)
  qqplot(data_n,data_m)
  abline(a = 0, b = 1, col = "steelblue")
  
  #d
  n = 25;
  m = 1000;
  
  data_uniforme_n = runif(n)
  data_uniforme_m = runif(m)
  hist(data_uniforme_n)
  hist(data_uniforme_m)
  qqplot(data_uniforme_n,data_uniforme_m)
  abline(a = 0, b = 1, col = "steelblue")
  
  #e
  n = 250;
  m = 1000;
  
  data_exp_n = rexp(n)
  data_exp_m = rexp(m)
  hist(data_exp_n)
  hist(data_exp_m)
  qqplot(data_exp_n,data_exp_m)
  abline(a = 0, b = 1, col = "steelblue")
  
  #f
  n = 100;
  
  #divido en 4 gráficos
  par(mfrow = c(1,4))
  
  qqplot(rgamma(n, 5, 1/2), rnorm(n))
  qqplot(rnorm(n)/ runif(n), rnorm(n))
  qqplot(runif(n), rnorm(n))
  qqplot(rexp(n), rnorm(n))
  # no hay relación entre la normal y los gráficos
  
  #vuelvo los gráficos a la normalidad
  par(mfrow = c(1,1))


##7##

  #a
  x <- rnorm(10)
  
  l2 = function(c) {
    r <- 0;
    for (i in 1:10) {
      r <- r + (x[i]-c)^2;
    }
    return(r)
  }
  
  plot(x, sapply(x,l2))
  abline(v=mean(x), col="blue")
  #Pareciera que se minimiza cando c= mean(data)
  
  #b
  l1 = function(c) {
    r <- 0;
    for (i in 1:10) {
      r <- r + abs(x[i]-c);
    }
    return(r)
  }
  
  plot(x, sapply(x,l1))
  abline(v=mean(x), col="blue")
  #Pareciera que se minimiza cando c= mean(data)


##9##

  library(readr)
  deptos <- read.table("DatosP6/departamentos2016.csv", ";",
                       escape_double = FALSE, trim_ws = TRUE)
  colnames(deptos)
  
  deptos <- read.table("DatosP6/departamentos2016.csv", header = TRUE)
  View(deptos)
  
  #a
  plot(departamentos$DOLARES, departamentos$M2 )
  #Son directamente proporcionales, mientras crece uno, crece el otro.
  
  #b
  covarianza = cov(departamentos$DOLARES, departamentos$M2)
  correlacion = cor(departamentos$DOLARES, departamentos$M2)
  
  #c)
  desvio_x = sd(departamentos$M2)
  media_x = mean(departamentos$M2)
  
  desvio_y = sd(departamentos$DOLARES)
  media_y = mean(departamentos$DOLARES)
  
  pendiente = desvio_y/desvio_x * correlacion
  ordenada =  media_y - pendiente*media_x
  
  #d
  plot(lm(formula = departamentos$DOLARES ~ departamentos$M2))
  # O sino 
  plot(departamentos$DOLARES, departamentos$M2)
  linea_regresion = lm(departamentos$DOLARES ~ departamentos$M2)
  abline(linea_regresion)
