#1.
  rendimiento <- c(37, 39.50, 41.70, 42, 40, 41.25, 43, 44.05, 38, 38.50)
  
  alpha <- 0.05
  n <- length(rendimiento)
  Xn <- mean(rendimiento)

  corte <- qnorm(1-alpha)
  
  z0 <- (Xn - 37)/sqrt(25/n)
  
  #Prob. error tipo 1
  EI <- 1 - pnorm(corte)
  
  #Prob. error tipo 2 con mu = 40
  z1 <- (37 - 40)/sqrt(25/n)
  EII <- pnorm(z1 + corte)
  
  ((qnorm(0.05)-corte)*5/(37-40))^2
  #para que P(EII) <= 0.05 con mu = 40, n >= 31

#2.a.  
  qnorm(0.45)
  
  1 - pnorm(4.899)
  
#3.a.
  tiempo <- c(27, 41, 22, 27, 23, 35, 30, 24, 27, 28, 22)

  S2 <- var(tiempo)
  
  #para rechazar H0 z_alpha > Tobs
  qchisq(0.05, 10) > S2*10/6^2     # false: no rehcazo H0
  
  #p-valor
  pchisq(S2*10/6^2, 10)
  

#4.
  qnorm(1-0.05)
  
  1 - pnorm(1.4142)

  pnorm(0.41796)
  
  (qnorm(0.05)*2/(1/3-3/sqrt(51)))^2
  
  pnorm(1.644854)

  qnorm(0.95)
  pnorm(0.37947)

#6.a
  setwd("C:/Users/maria/Desktop/Taller/Proba/")
  dados <- scan("DatosP9/dado.txt")
  
  # Distribución desconocida y H0: mu = 1/2 vs. H1: mu != 1/2
  alpha <- 0.05
  mu0 <- 1/2
  Xn <- mean(dados %% 2  == 0)
  n <- length(dados)
  S <- sd(dados)
  
  z1 <- qnorm(alpha/2)
  z2 <- qnorm(1-alpha/2)

  Z <- (Xn - mu0)*sqrt(n)/S
  
  #Rechazamos H0
  Z <= z1 || z2 <= Z # false, no rechazamos H0
  
  #p-valor
  p_valor <- 2*(1-pnorm(Z)) #p-valor > alpha, no rechazamos H0
  
  #No hay evidencia suficiente para rechazar H0 en favor de H1
  
#6.b
  # Distribución desconocida y H0: mu = 1/2 vs. H1: mu != 1/2
  alpha <- 0.05
  mu0 <- 1/2
  Xn <- mean(dados <= 3)
  n <- length(dados)
  S <- sd(dados)
  
  z1 <- qnorm(alpha/2)
  z2 <- qnorm(1-alpha/2)
  
  Z <- (Xn - mu0)*sqrt(n)/S
  
  #Rechazamos H0
  Z <= z1 || z2 <= Z # false, no rechazamos H0
  
  #p-valor
  p_valor <- 2*pnorm(Z) #p-valor > alpha, no rechazamos H0
  
  #No hay evidencia suficiente para rechazar H0 en favor de H1
  
#6.c
  # Distribución desconocida y H0: mu = 1/6 vs. H1: mu != 1/6
  alpha <- 0.05
  mu0 <- 1/6
  Xn <- mean(dados == 3)
  n <- length(dados)
  S <- sd(dados)
  
  z1 <- qnorm(alpha/2)
  z2 <- qnorm(1-alpha/2)
  
  Z <- (Xn - mu0)*sqrt(n)/S
  
  #Rechazamos H0
  Z <= z1 || z2 <= Z # false, no rechazamos H0
  
  #p-valor
  p_valor <- 2*pnorm(Z) #p-valor > alpha, no rechazamos H0
  
  #No hay evidencia suficiente para rechazar H0 en favor de H1
  
#6.d
  # Distribución desconocida y H0: mu = 1/6 vs. H1: mu != 1/6
  alpha <- 0.05
  mu0 <- 1/6
  Xn <- mean(dados == 2)
  n <- length(dados)
  S <- sd(dados)
  
  z1 <- qnorm(alpha/2)
  z2 <- qnorm(1-alpha/2)
  
  Z <- (Xn - mu0)*sqrt(n)/S
  
  #Rechazamos H0
  Z <= z1 || z2 <= Z # false, no rechazamos H0
  
  #p-valor
  p_valor <- 2*(1-pnorm(Z)) #p-valor > alpha, no rechazamos H0
  
  #No hay evidencia suficiente para rechazar H0 en favor de H1
  
#6.e. No hay evidencia suficiente a nivel asintótico 0.05 de que el dado está cargado