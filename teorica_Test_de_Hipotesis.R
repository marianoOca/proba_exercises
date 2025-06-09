#Ejercicio 1: Grado de impureza de un producto químico
  #Mediciones:
  mu0 <- 0.7
  n_med <- 12
  alpha <- 0.05
  promedio_obs <- 0.85
  desvio_obs <- 0.05
  
  t_obs <- (promedio_obs-mu0)*sqrt(n_med)/desvio_obs
  corte_t <- qt(1-alpha/2, n_med-1)
  
  #¿Estamos en la región de rechazo?
  abs(t_obs) >= corte_t
  
  #calculamos p-valor
  valor_p_med <- 2*(1-pt(abs(t_obs), n_med-1))


#Ejercicio 2.a. Riego H1 : sigma^2 < 36
  datos <- scan()
  27 41 22 27 23 35 30 24 27 28 22
  
  var0 <- 36
  alpha <- 0.05
  n <- length(datos)
  S_cuadrado <- var(datos)

  xi_obs <- (n-1)*S_cuadrado/var0
  corte_xi <- qchisq(alpha, n-1)
  
  #¿Estamos en la región de rechazo?
  abs(xi_obs) <= corte_xi

  #no estamos en la región de rechazo por lo que
  #no hay evidencias para decir que el desvio del nuevo
  #diseño es menor a 6
  
  #calculamos p-valor
  p_valor <- pchisq(xi_obs, n-1)
  
#Ejercicio 2.b. Riego H1 : sigma^2 != 36
  corte_inf <- qchisq(alpha/2, n-1)
  corte_sup <- qchisq(1-alpha/2, n-1)
  
  #Rechazamos H0
  xi_obs <= corte_inf || corte_sup <= xi_obs

  #no estamos en la región de rechazo por lo que no hay
  #evidencia para rechazar H0
  
  #calculamos p-valor:
  #buscamos el área más chica y lo multimplicamos por 2
  p_valor <- 2*(min(pchisq(xi_obs, n-1), 1-pchisq(xi_obs, n-1)))
  
#Ejercicio 3: Los dueños que se parecen a sus mascotas
  #Escenario 1
  p0 <- 1/2
  n <- 25
  exitos <- 15
  alpha <- 0.05
  p_obs <- exitos/n                     #0.6
  
  z_obs <- (p_obs-p0)/sqrt(p0*(1-p0)/n) #1
  corte_z <- qnorm(1-alpha)             #1.644854
  
  #¿Estamos en la región de rechazo?
  z_obs >= corte_z                      #FALSE, NO rechazo H0
  
  p_valor <- 1-pnorm(z_obs)             #0.1586553
  
  #Escenario 2
  p0 <- 1/2
  n <- 250
  exitos <- 150
  alpha <- 0.05
  p_obs <- exitos/n                      #0.6
  
  z_obs <- (p_obs-p0)/sqrt (p0*(1-p0)/n) #3.162278
  corte_z <- qnorm(1-alpha)              #1.644854
  
  #¿Estamos en la región de rechazo?
  z_obs>=corte_z                         #TRUE, rechazo H0
  
  p_valor <- 1-qnorm(z_obs)              #0.0007827011

#Ejercicio 4: ¿Tienen hombres y mujeres la misma opinión sobre el divorcio?
  n1 <- 1029
  n2 <- 1029
  p1 <- 0.71                     #hombres
  p2 <- 0.67                     #mujeres
  
  incertidumbre <- sqrt (p1*(1-p1)/n1+p2*(1-p2)/n2)
  z_obs <- (pl-p2)/incertidumbre
  
  #rechazo a nivel 0.05?
  
  alpha <- 0.05
  
  corte_z <- qnorm(1-alpha/2)       #es a dos colas
  
  #estamos en la region de rechazo?
  
  abs(z_obs)>corte_z                #TRUE, rechaamos HO.
  
  p_valor <- 2*(1-pnorm(abs(z_obs))) #0.04957612