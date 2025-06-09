#Ejercicio 1
x <- c(0,1,2,3)
#a-
px <- rep(0,4)
px[1] <- 11^3/15^3
px[2] <- 11^2*3*4/15^3
px[3] <- 4*4*11*3/15^3
px[4] <- 4^3/15^3
plot(x,px)

#c-
Fx <- c(px[1],px[1]+px[2],px[1]+px[2]+px[3],px[1]+px[2]+px[3]+px[4])
plot(x,Fx)

#d-
lote <- c(rep(0,11),rep(1,4))
extracción <- sample(lote,3,replace = TRUE)

p_X_0 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==0)
p_X_1 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==1)
p_X_2 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==2)
p_X_3 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==3)

#Ejercicio 2

#c-
espacio <- c(rep(1,3),rep(3,1),rep(6,2),rep(12,4))
sample(espacio,5,replace = TRUE)

#d-
muestra <- sample(espacio,1000000,replace = TRUE)

mean(muestra > 3 & muestra <= 6)
mean(muestra >= 3 & muestra <= 6)
mean(muestra >= 4)
mean(muestra >= 6)

#Ejercio 3 

Fx <- function(x,p,t){
      fda <- 0
        for(i in 1:(length(x))){
          if(x[i]<=t){
            fda = fda + p[i]}
        }
      fda
      }

x <- c(1,3,6,12)
p <- c(0.3, 0.1, 0.2, 0.4)
Fx(x,p,8) #debería devolver 0.6

#Ejercicio 5
p_X_0 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==0)
p_X_1 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==1)
p_X_2 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==2)
p_X_3 <- mean(replicate(10000, sum(sample(lote,3,replace=TRUE)))==3)
E <-p_X_0 * 0 + p_X_1 * 1 + p_X_2 * 2 + p_X_3 * 3

#Ejercicio 6
Esperanza <- function(x,p){
  Esperancita <- 0
  for(i in 1:(length(x))){
      Esperancita = Esperancita + x[i]*p[i]
  }
  Esperancita
}
x <- c(0:3)
Esperanza(x,px)

#Ejercicio 7
varianza <- function(x,p){
    v = Esperanza((x-1)*(x-1),p)
    v
}
x <- c(0:3)
varianza(x,px)

#Ejercicio 17
x <- c(0:6)
px <- c(dpois(0,4),dpois(1,4),dpois(2,4),dpois(3,4),dpois(4,4),dpois(5,4),dpois(6,4))
50*Esperanza(x,px)-100

#Ejercicio 18
#a-

n <- 5
p <- 1/3
x <- seq(0, n)

fpp <-dbinom(x,n,p)
sum(dbinom(x,n,p))
barplot(fpp,col = "pink",ylim = c(0,.4),names.arg=x,
        main="Binomial Distribution\n(n=5,p=1/3)")

fda <- pbinom(x,n,p)
plot(x,fda)
barplot(fda,col = "red",ylim = c(0,1),names.arg=x,
        main="Binomial Distribution\n(n=5,p=1/3)")
distTable = cbind(x,fpp,fda)

#b-
fda2 <- c(Fx(x,fpp,0),Fx(x,fpp,1),Fx(x,fpp,2),Fx(x,fpp,3),Fx(x,fpp,4),Fx(x,fpp,5))
barplot(fda2,col = "blue",ylim = c(0,1),names.arg=x,
        main="Binomial Distribution\n(n=5,p=1/3)")
plot(x,fda2)
fda
fda2

#c-
a <- rbinom(10000,n,p)
sum(rbinom(10000,n,p)==0)/10000
sum(rbinom(10000,n,p)==1)/10000
sum(rbinom(10000,n,p)==2)/10000
sum(rbinom(10000,n,p)==3)/10000
sum(rbinom(10000,n,p)==4)/10000
sum(rbinom(10000,n,p)==5)/10000

#Ejercicio 19
#a-
ppois(5,2.5) - dpois(5,2.5)
ppois(5,2.5)
dpois(5,2.5)
help("ppois")

#b-
p_menor_igual_6 <-  ppois(6,5)
sum(rpois(100000,5)<=6)/100000

p_igual_6 <- dpois(6,5)
sum(rpois(10000000,5)==6)/10000000

p_mayor_igual_5 <- 1 - ppois(4,5) 
sum(rpois(10000000,5)>=5)/10000000

p_entre_3_y_6_incluidos <- sum(dpois(c(3:6),5))
sum((rpois(10000000,5)>=3) & (rpois(10000000,5)<=6))/10000000

p_entre_3_y_6 <- p_menor_igual_6 -p_igual_6 - ppois(3,5)
sum((rpois(100000000,5)>3) & (rpois(100000000,5)<6))/100000000

p_entre_3_y_6_inc_dado_p_mayor_ig_4 <- sum(dpois(c(4:6),5))/(1 - ppois(3,5) )


#d-
n <- c(0:30)
Esperanza <- sum(rpois(1000000,30*5))*(1/1000000)

