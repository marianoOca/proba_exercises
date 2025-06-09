library(PASWR2)
View(orange)
#en donde dice c() pongan los numeros que les den.
ora<-Orange[-c(5,6,8,13),]

View(ora)

#a-
mean(ora$age)

#b-
sd(ora$circumference)

#c-
median(ora$age[ora$Tree==2])


#d-
x <- ora$circumference
y <- ora$age
regresion <- lm(y ~ x, data = ora)

#la rta es, cuando corran regresion[["coefficients"]], 
#pongan el que esta abajo de x, no de intercept.
regresion[["coefficients"]]

#e- el que les dio en el d, por el numero que les pasan, más es que el estaba
#abajo de intercept
7.810527 *86 + 12.901800