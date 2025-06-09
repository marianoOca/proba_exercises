#Ej 1.d)
fx = function(x){(3/4)*(1-x*x)}
X <- seq(-1,1,0.01)
plot(fx(-1:1), type='l')

#Ej 2.e)
Fx = function(x){x*x*x/8}
fx = function(x){(3/8)*x*x}
X <- seq(-1,1,0.01)
plot(Fx(0:2), type='l')

#Ej 


#con X~N(0,1)
  #calcular P(X < algo) o P(X <= algo)
  pnorm(3)-pnorm(1)
  pnorm((78-74.34822)/3.22832)
  #calcular  el algo-percentil
  qnorm(1)
  
  A <- pnorm(-0.4)-pnorm(-2)
  B <- 1 - pnorm(0.4)- 1 + pnorm(2)
  C <- pnorm(1) - 1 + pnorm(.4)  
  D <- 1-pnorm(1)
  
  dpois(1,3)*dpois(1,2)/dpois(2,5)
  
  pnorm(1) - pnorm(-1)
  
  dmultinom()