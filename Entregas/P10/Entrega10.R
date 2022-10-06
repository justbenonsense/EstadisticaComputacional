#' -------------------------------------------------------------------------------------
#' Carlota Valdivia Manzano
#' Estadistica Computacional
#' Curso 2021-2022
#' --------------------------------------------------------------------------------------

#' --------------------------------ENTREGA 10---------------------------------------------

#' --Ejercicio 1-------------------------------------------------------------------------

nsim<-1000
set.seed(1)
x<-runif(nsim,-1,1)
y<-runif(nsim,-1,1)
suceso<-(x^2+y^2<=1)

#' Aproximación
aprox<-mean(suceso)

#' Error de estimación
sd(suceso)/sqrt(nsim)

#' Gráfico de convergencia
#' Aproximaciones para $n=1,...,nsim$
estim<-cumsum(suceso)/(1:nsim)

#' Errores de estimación correspondientes
estim.err<-sqrt(cumsum((suceso-estim)^2))/(1:nsim)

plot(1:nsim,estim,type='l',ylab="Aproximación y límites de error", xlab="Número de simulaciones", main=expression(P(X^2+Y^2<=1),ylim=c(0,1)))

z<-qnorm(0.025,lower.tail = FALSE)
lines(estim - z*estim.err, col="blue", lwd=2,lty=3)
lines(estim + z*estim.err, col="blue", lwd=2,lty=3)

#' Vemos cual sería el valor real y su diferencia
value <- pi / 4.0
abs(value - aprox)

#' Borramos memoria

rm(list=ls(all=TRUE))


#' --Ejercicio 2-------------------------------------------------------------------------


#' ----------Primera integral-----------------------------------------------------
f1<-function(x) dbeta(x,2.5,5)
curve(f1(x),0.2,0.4)

#' Aproximamos las dos integrales anteriores usando simulación
#' Consideramos 1000 simulaciones
nsim<-1000

#' Calculamos la aproximación
set.seed(1)
x<-runif(nsim)
f1x<-sapply(x,f1)

mean(f1x)

#' Aproximaciones para $n=1,...,nsim$
estim1<-cumsum(f1x)/(1:nsim)

#' Errores de estimación correspondientes
estim1.err<-sqrt(cumsum((f1x-estim1)^2))/(1:nsim)


#' Construimos el gráfico que muestre la convergencia junto con los límites de error
plot(1:nsim,estim1,type='l',ylab='Aproximación y límites de error', xlab='Número de simulaciones')

z<-qnorm(0.025,lower.tail = FALSE)

lines(estim1 - z * estim1.err, col = "blue", lwd = 2, lty = 3)
lines(estim1 + z * estim1.err, col = "blue", lwd = 2, lty = 3)



#' ----------Segunda integral-----------------------------------------------------
f2<-function(x) sin(x)*exp(-x)*dbeta(x,2.5,5)
curve(f2(x),0,1)

#' Aproximamos las dos integrales anteriores usando simulación
#' Consideramos 1000 simulaciones
nsim<-1000

#' Definimos las integrales
F2<-function(x) sin(x)*exp(-x)*dbeta(x,2.5,5) * (x>0 & x<1)

#' Calculamos la aproximación
set.seed(1)
x<-runif(nsim)
f2x<-sapply(x,f2)

mean(f2x)

#' Aproximaciones para $n=1,...,nsim$
estim2<-cumsum(f2x)/(1:nsim)

#' Errores de estimación correspondientes
estim2.err<-sqrt(cumsum((f1x-estim2)^2))/(1:nsim)


#' Construimos el gráfico que muestre la convergencia junto con los límites de error
plot(1:nsim,estim2,type='l',ylab='Aproximación y límites de error', xlab='Número de simulaciones')

z<-qnorm(0.025,lower.tail = FALSE)

lines(estim2 - z * estim2.err, col = "blue", lwd = 2, lty = 3)
lines(estim2 + z * estim2.err, col = "blue", lwd = 2, lty = 3)

#' Comparamos las aproximaciones obtenidas con las aproximaciones numericas 
#' que calcula la funcion integrate
#' Primera integral
f1<-function(x) dbeta(x,2.5,5)
integrate(f1,0.2,0.4)

#' También podemos evaluarla con la función pbeta()
pbeta(0.4,2.5,5)-pbeta(0.2,2.5,5)

#' Segunda integral
f2<-function(x) sin(x)*exp(-x)*dbeta(x,2.5,5)
integrate(f2,-Inf,Inf)

#' Borramos memoria

rm(list=ls(all=TRUE))


#' --Ejercicio 3-------------------------------------------------------------------------

#' Parámetros de la lognormal (X)
mu<-3.5
sig<-1.1

#' Calculamos E[X] y V(X)
EX<-exp(mu+sig^2/2) 
EX

VX<-EX^2*(exp(sig^2)-1)
VX

#' Parámetro de la Poisson (N)
lam<-17 #' coincide con E[N] y V(N)

#' Media de la Poisson compuesta
ES<-lam*EX 
ES

#' Varianza de la Poisson compuesta
VS<-lam*VX + lam*EX^2 
VS

#' Asignamos el número de simulaciones
nsim<-5000 

#' Simulamos ahora nsim valores de S_N:
S <- double(nsim) #' para almacenar los valores simulados
set.seed(1)
for (i in 1:nsim) {
    n <- rpois(1,lam)
    if (n>0) S[i] <- sum(rlnorm(n,mu,sig))
}

#'  S contiene los valores simulados de S_N
#' Un histograma de S nos da una aproximación de la distribución
#' Superponemos la densidad suavizada que calcula density()
hist(S,xlim=c(0,7000),breaks=20,prob=TRUE, ylim=c(0,1.2e-3))
lines(density(S),col="red")

#' Calculamos la media yla varianza de los nsim valores simulados
mean(S)
var(S)

#' Vemos que son muy parecido a E_S y V_S

#' Aproximamos el VaR
quantile(S, 0.995)

#' Aproximación por una Normal con media ES y varianza VS
qnorm(0.995, mean=ES, sd=sqrt(VS))


#' Borramos memoria

rm(list=ls(all=TRUE))

