#1.e.b.1
  setwd("C:/Users/maria/Desktop/Taller/Proba/")
  datos10 <- read.table("DatosP8/datosP8ej1_10.csv", header = TRUE)
  datos1000 <- read.table("DatosP8/datosP8ej1_1000.csv", header = TRUE)
  View(datos10)  
  
  alpha <- 0.05
  z <- qnorm(1-alpha/2)
  
  #con sólo 10 datos
  n <- length(datos10[,1])
  sigma2 <- var(datos10[,1])
  
  longitudIntervalo10 <- 2*z*sqrt(sigma2/n)
  
  #con 1000 datos en vez de 10
  n <- length(datos1000[,1])
  sigma2 <- var(datos1000[,1])
  
  longitudIntervalo1000 <- 2*z*sqrt(sigma2/n)

#2. jajajajaja
  
#4.a
  #Distribución t-student (con 4 grados de libertad)
  alpha <- 0.05
  t <- qt(alpha/2, 4)
  qt(1-alpha/2, 4)
  
  525 + t * sqrt(100/5)
  525 - t * sqrt(100/5)

#5.b.
  varillas <- c(176.50, 191.50, 186.90, 181.10, 195.70, 188.10, 187.40, 185.10, 176.90,
                201.00, 192.50, 176.60, 191.20, 193.80, 187.00, 179.00, 173.00, 184.40,
                199.60, 190.40, 206.80, 193.00, 177.10, 180.10, 186.40)

  nivel <- 0.90
  alpha <- 1 - nivel
  mu <- 185
  
  #Distribución Chi-cuadrado (con 25 grados de libertad)
  qchisq(1-alpha/2, 25)
  qchisq(alpha/2, 25)

  #función en SandBox.R
  intVarConMu(mu, alpha, varillas)

#6.b. 
  #función en SandBox.R
  intVarSinMu(alpha, varillas)
  
#8.d
  lamps <- c(39.08, 45.27, 26.27, 14.77, 65.84, 49.64, 0.80, 66.58, 69.60, 32.42,
             228.36, 64.79, 9.38, 3.86, 37.18, 104.75, 3.64, 104.19, 8.17, 8.36)
  
  nivel <- 0.95
  alpha <- 1 - nivel
  
  intExp(alpha, lamps)
  
#9.b.
  mu <- 0.6
  n <- 1000
  z <- qnorm(0.05)
  
  c(mu+z*sqrt(mu*(1-mu)/n), mu-z*sqrt(mu*(1-mu)/n))
  
#9.c.
  z <- qnorm(1-0.05)
  
  (mu*(1-mu))*(2*z/0.02)^2
  
#9.d.
  encuestaA <- read.table("DatosP8/datosP8ej9_1000.csv", header = TRUE)
  encuestaB <- read.table("DatosP8/datosP8ej9_6494.csv", header = TRUE)
  
  nivelAsintotico <- 0.90
  alpha <- 1 - nivelAsintotico
  
  intervaloA <- intBin(alpha, encuestaA == "en contra")
  largoA = intervaloA[2] - intervaloA[1]
  largoA
  
  intervaloB <- intBin(alpha, encuestaB == "en contra")
  largoB = intervaloB[2] - intervaloB[1]
  largoB
  
#11.b.
  llamadas <- c(40, 46, 46, 45, 42, 44, 50, 31, 41, 42, 50, 34, 62, 32, 46, 50,
             39, 42, 44, 41, 47, 42, 41, 50, 34, 47, 38, 40, 44, 45, 35, 51,
             38, 41, 39, 34, 48, 35, 40, 40, 43, 36, 40, 49, 45, 47, 34, 45)
  
  alpha <- 0.05
  
  intPoi(alpha, llamadas)
  
#13.
  X <- c(0.44, -1.63, 2.59, 1.54, 0.45, -0.13, -2.76, -1.53)
  Y <- c(0.06, -0.24, 4.65, 2.27, 3.88, 2.35, 3.92, -0.73)

  nivel <- 0.95
  alpha <- 1 - nivel
  sigma2 <- 0.1*0.1
  
  qnorm(1-0.025)

  intMuConVar(sigma2, alpha, X)
  
  intMuConVar(sigma2, alpha, Y)
  
  mu <- mean(X)-mean(Y)
  n <- length(X)
  z <- qnorm(alpha/2)
  sigma2 <- 0.1*0.1 * 2
  c(mu+z*sqrt(sigma2/n), mu-z*sqrt(sigma2/n))