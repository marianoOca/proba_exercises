#1.
  panes <- c(75.09, 76.39, 76.42, 76.50, 76.68, 76.88, 
             76.88, 76.93, 77.07, 77.16, 77.67, 78.15)
  
  #a.
  mean(panes)
  
  #b.
  median(panes)
  
  quantile(panes, 0.5)
  
  #c
  mean(panes <= 76.5)

#3.b.
  obsFisica <- c(25.11, 25.02, 25.16, 24.98, 24.83, 25.05, 24.94, 25.04, 24.99, 24.96,
                 25.03, 24.97, 24.93, 25.12, 25.01, 25.12, 24.90, 24.98, 25.10, 24.96)
  
  mean(obsFisica)  
  
  #c.
  obs <- c(12.51, 11.66, 11.91, 12.25, 11.54, 11.36, 12.40, 12.19, 12.88, 12.16, 12.69, 12.91,
           12.12, 11.02, 12.53, 11.77, 12.72, 10.56, 11.52, 11.66, 12.25, 12.09, 11.48, 12.36)
  
  varNormal = function(X, mu){
    res <- 0
    for(i in 1:length(X)){
      res <- res + (X[i]-mu)^2
    }
    return <- res / length(X)
  }  
  
  x <- varNormal(obs, 12)
  x
  
#4.d.
  lamps <- c(39.08, 45.27, 26.27, 14.77, 65.84, 49.64, 0.80, 66.58, 69.60, 32.42,
             228.36, 64.79, 9.38, 3.86, 37.18, 104.75, 3.64, 104.19, 8.17, 8.36)
  
  #i
  tita <- 1/mean(lamps)
  tita
  
  quantile(lamps, 0.9)
  -log(.1)*mean(lamps)
  
  #e.
  setwd("C:/Users/maria/Desktop/Taller/Proba/")
  lamps <- scan("DatosP7/lamparas2.txt")
  
  tita <- 1/mean(lamps)
  tita
  
  quantile(lamps, 0.9)
  -log(.1)*mean(lamps)

#5.d.
  llama <- c(40, 46, 46, 45, 42, 44, 50, 31, 41, 42, 50, 34, 62, 32, 46, 50,
             39, 42, 44, 41, 47, 42, 41, 50, 34, 47, 38, 40, 44, 45, 35, 51,
             38, 41, 39, 34, 48, 35, 40, 40, 43, 36, 40, 49, 45, 47, 34, 45)
  
  mean(llama)
  
  mean(llama == 40)
  
  #e.
  llama <- scan("DatosP7/llamadas.txt")
  
  mean(llama)
  
  mean(llama == 40)
  
  qnorm(.025)

#6.d.
  ejes <- c(0.5, 0.7, 0.8, 0.95, 0.9, 0.6, 0.2, 0.85, 0.3, 0.2,
            0.76, 0.55, 0.48, 0.8, 0.76, 0.13, 0.15, 0.67, 0.9, 0.95)
  #con Maxima Verosimilitud
  sum(log(ejes))/length(ejes) 
  
#12.
  dias <- scan("DatosP7/dias.txt")

  #puedo llamar a la función fitdistr del pakage MASS
  MASS::fitdistr(dias, densfun = "negative binomial") 
  
  #o cargarlo directamente en memoria
  library("MASS")
  