tabla <- matrix(
  c(#80    90    100   120   130  px()
    0.13, 0.02, 0.11, 0.03, 0.00, #0.29
    0.06, 0.01, 0.04, 0.02, 0.00, #0.13
    0.08, 0.03, 0.24, 0.02, 0.03),#0.58
#py 0.27  0.06  0.39  0.25  0.03    1
  nrow=3, byrow = TRUE)

#E(XY)
EXY <- 80*100*0.13 + 90*100*0.02 + 100*100*0.11 + 120*100*0.03 + 130*100*0.00 +
       80*120*0.06 + 90*120*0.01 + 100*120*0.04 + 120*120*0.02 + 130*120*0.00 +
       80*140*0.08 + 90*140*0.03 + 100*140*0.24 + 120*140*0.02 + 130*140*0.03
#E(X)
EX <- 100*0.29 + 120*0.13 + 140*0.58
#E(Y)
EY <- 80*0.27 + 90*0.06 + 100*0.39 + 120*0.25 + 130*0.03
#Cov(X,Y)
CovXY <- EXY - EX*EY

#Esto quiere decir que en las panaderías de esta población, cuando el pan es más caro que el
#promedio, también lo es la docena de facturas.

#V(X)
VX <- sum(c(100,120,140)^2*rowSums(tabla))-(sum(c(100,120,140)*rowSums(tabla)))^2
#V(Y)
VY <- sum(c(80,90,100,120,130)^2*colSums(tabla))-sum(c(80,90,100,120,130)*colSums(tabla))^2
#Correlación
ro <- CovXY / sqrt(VX*VY)
