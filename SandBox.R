#q antes de la funci¨®n indica la inversa de la acumulada

#redondear con 2 decimales
round(qexp(0.5, 1), 2)


###Distribuciones Discretas


##BERNULLI:  Be(p) --> p = prob de Exito
  #E(x) = p
  #V(x) = p*(1-p)


##BINOMIAL:  Bi(n, p) --> repito n veces un Be(p)
  dbinom(k, n, p)  # P(x = k) = f(k) = ?   
  pbinom(k, n, p)  # P(x <= k) = F(k) = ?
  qbinom(q, n, p)  # P(x <= ?) = q (percentil)
  #E(x) = n*p
  #V(x) = n*p*(1-p)


##GEOMETRICA:  ge(p) --> repito Be(p) hasta obtener 1 Exito (puede dar distinto)
  dgeom(k, p)  # P(x = k) = f(k) = ?
  pgeom(k, p)  # P(x <= k) = F(k) = ?
  #E(x) = 1/p
  #V(x) = (1-p)/(p^2)
  

##BINOMIAL NEGAIVA:  BN(r, p) -->  repito Be(p) hasta obtener r Exitos (esta tambi¨¦n)
  dnbinom(k, r, p)  # P(x = k) = f(k) = ?
  pnbinom(k, r, p)  # P(x <= k) = F(k) = ?
  qnbinom(q, r, p)  # P(x <= ?) = q (percentil)
  #E(x) = r/p
  #V(x) = r*(1-p)/(p^2)
  
##POISSON: P(l)  l = n*p --> Bi(n,p) con n muy grande y p muy chico
  dpois(k, l)  # P(x = k) = f(k) = ?   
  ppois(k, l)  # P(x <= k) = F(k) = ?
  #E(x) = V(x) = l


##HYPERGEOMETRICA:  H(n, N, D) --> Hay N opciones, D favorables y extraigo n elementos
  dhyper(k, B, N-B, n)  #P(x = k)
  dhyper(2, 14, 11, 2)
  #E(x) = n*D/N


##MULTINOMIAL
  #pasa 7 veces A con ProbA = 0.4, 
  #pasa 2 veces B con ProbB = 0.35 y
  #pasa 3 veces C con ProbC = 0.25.
  dmultinom(x=c(7,2,3), prob = c(0.4,0.35,0.25))


###Distribuciones Contínuas


##UNIFORME: U(a, b)
  punif(X, a, b)  # Fx(X) = (X-a)/(b-a) = ?
  qunif(q, a, b)  # (?-A)/(B-A) = q
  #E(x) = (a+b)/2
  #V(x) = (b-a)^2 /12


##NORMAL:  N(0,1) --> N(u, d^2)
  pnorm(c, 0, 1)  # P(z <= c) = ?
  qnorm(n, 0, 1)  # P(z <= ?) = n
  #E(x) = u
  #V(x) = d^2


##EXPONENCIAL:  E(l)
  dexp(t, l)  # fx(t) = l*e^(-t*l) = ?
  pexp(t, l)  # Fx(t) = P(x<=t) = 1 - P(x>t) = 1 - e^(-t*l) = ?
  qexp(q, l)  # Fx(?) = P(x<=?) = q
  #E(x) = 1/l
  #V(x) = 1/l^2


##GAMMA:  r(a, l)
  dgamma(t, a, l)  #fx(t) = ?
  pgamma(t, a, l)  #Fx(t) = P(x<=t) = ?
  qgamma(q, a, l)  #Fx(?) = P(x<=?) = q


  
###Intervalos de Confianza

  
##calcula S^2 es igual a calcular var(datos)
  
##recordar que si
  nivel <- 0.95
  alpha <- 1 - nivel
  
##Intervalo de confianza asintótico para una distribución desconocida (n > 30)
  intMuMisterio <- function(alpha, datos){
    Xn <- mean(datos)
    S2 <- var(datos)
    n <- length(datos)
    z <- qnorm(alpha/2)
    
    return(c(Xn+z*sqrt(S2/n), Xn-z*sqrt(S2/n)))
    
  }
  
##Intervalo de confianza de la esperanza (mu) sabiendo la varianza (sigma)
  intMuConVar <- function(sigma2, alpha, datos){
    Xn <- mean(datos)
    n <- length(datos)
    z <- qnorm(alpha/2)
    
    return(c(Xn+z*sqrt(sigma2/n), Xn-z*sqrt(sigma2/n)))
  }
  
##Intervalo de confianza de la esperanza sin saber la varianza
  intMuSinVar <- function(alpha, datos){
    Xn <- mean(datos)
    S2 <- var(datos)
    n0 <- length(datos)-1
    t <- qt(alpha/2, n0)
    
    return(c(Xn+t*sqrt(S2/n0), Xn-t*sqrt(S2/n0)))
    
  }
  
##Intervalo de confianza de la varianza sabiendo la esperanza
  intVarConMu <- function(mu, alpha, datos){
    n <- length(datos)
    x1 <- qchisq(1-alpha/2, n)
    x2 <- qchisq(alpha/2, n)
    
    res <- 0
    for( i in 1:n){
      res <- res + (datos[i] - mu)^2
    }
    
    return(c(res/x1, res/x2))
  }
    
##Intervalo de confianza de la varianza sin saber la esperanza
  intVarSinMu <- function(alpha, datos){
    S2 <- var(datos)
    n0 <- length(datos)-1
    x1 <- qchisq(1-alpha/2, n0)
    x2 <- qchisq(alpha/2, n0)
    
    return(c(n0*S2/x1, n0*S2/x2))
  }
  
##Intervalo de confianza para b de una U(0, b)
  intUni <- function(alpha, datos){
    n <- length(datos)
    mayor <- max(datos)
    return(c(mayor, mayor/(alpha^(1/n))))
  }

##Intervalo de confianza para lambda de una Exponencial
  intExp <- function(alpha, datos){
    n2 <- 2 * length(datos)
    suma2 <- 2 * sum(datos)
    x1 <- qchisq(alpha/2, n2)
    x2 <- qchisq(1-alpha/2, n2)

    return(c(x1/suma2, x2/suma2))
  }
  
##Intervalo de confianza asintótico para p de una Binomial (n > 30)
  intBin <- function(alpha, datos){
    mu <- mean(datos)
    n <- length(datos)
    z <- qnorm(alpha/2)
    
    return( c(mu+z*sqrt(mu*(1-mu)/n), mu-z*sqrt(mu*(1-mu)/n)) )
  }

##Intervalo de confianza asintótico para lamda de una Poisson (n > 30)
  intPoi <- function(alpha, datos){
    mu <- mean(datos)
    n <- length(datos)
    z <- qnorm(alpha/2)
    
    return(c(mu+z*sqrt(mu/n), mu-z*sqrt(mu/n)))
  }



###SandBox

  1 - pexp(10, 0.0415)

  qexp(0.5, 0.0415)

  1 - pnorm(0.81)
  
  corte <- qnorm(0.05/2)

  z <- (210-230)/sqrt(36/80)

  pnorm(z-corte)
  pnorm(z+corte)

  
  qnorm(0.01)

  
  pnorm(207.7356, 210, 36/38)
  
  1 - pnorm(212.2643, 210, 36/38)

  x <- 46/70
  x + qnorm(1 - 0.17) * sqrt(x*(1-x)/70)
  
  2*(qnorm(1 - 0.17) * sqrt(x*(1-x)/912))
  
  
  25.01 + qnorm(1-0.025)*sqrt(0.6/20)

  
  qnorm(0.95)  

  
  1- pnorm(2.213)  

  pnorm(-.2524)

  (2*qnorm(0.05)*5/3  )^2
  
  25.01 - qt(0.025, 19)*0.76/sqrt(20)  
  
  (2*qt(0.025, 19)*0.76/0.05)^2

  
  qnorm(0.5-0.05, 00, 1)
  
  qt(.025, 4)

  qnorm(0.95)
  
  1 - pnorm(1.414)
  
  1 - pnorm(0.002991)
  
  1-pnorm(sqrt(2.4))

  d <- scan()  
  0.5 0.7 0.8 0.95 0.9 0.6 0.2 0.85 0.3 0.2 0.76 0.55 0.48 0.8 0.76 0.13 0.15 0.67 0.9 0.95
  
  1/mean(d)-1

  -sum(log(d))/20  
  
  qnorm(0.05)

  .6 - qnorm(0.05)*sqrt(.6*.4/1000)
  
  (qnorm(1-0.05)/0.02)^2

  qchisq(0.95, 80)  

  qnorm(0.95)  

  1 - pnorm(0.5792)  

  qnorm(.025)

  1 - pnorm(0.804984)    

  2.875/qnorm(0.76)  

  
  #1
  2*(1-pnorm(1.70016))  

  1-pnorm(1.70016) 

  
  #2
  datos <- c(25, 39, 74, 72, 75)
  
  mean(datos)  
  
  var(datos) 
  sum((datos-57)^2)/5
  
  #4
  1- pnorm(1.035098)

  qnorm(0.04)  
  
  #5
  qnorm(0.025)
  
  1-pnorm(1.38086)
  
  qnorm(1-0.1)
  
  
  
  
  
  
  
  
  