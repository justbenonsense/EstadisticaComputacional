#' -------------------------------------------------------------------------------------
#' Carlota Valdivia Manzano
#' Estadistica Computacional
#' Curso 2021-2022
#' --------------------------------------------------------------------------------------

#' -------------------------------REPASO PARCIAL 2---------------------------------------

#' --Ejercicio 1-------------------------------------------------------------------------

#' Establecemos la semilla del generador de números aleatorios
set.seed(1)

#' Generamos 50 valores desde una distribución Chi-cuadrado con n= 30 grados de libertad
#' y lo almacenamos en un vector y
y<-rchisq(50, df = 30)

#' Generamos 50 valores desde una distribución Normal con media 30 y desviación típica 5
#' y lo almacenamos en un vector x
x<-rnorm(50, mean = 30, sd = 5)

#' Calculamos la media, la desviación típica y los tres cuartiles de y
mean_y<-mean(y)
sd_y<-sd(y)
quantile(y, c(0.25,0.5,0.75))

#' Representamos un histograma de los datos almacenados en y
hist(y, breaks="FD",freq=FALSE,main="Muestra Chi-cuadrado")

#' Suporponemos al histograma anterior la curva de la densidad Normal con media y desviación
#' típica la de los datos.
lines(density(x),col='blue')
curve(dnorm(x,mean=30, sd=5),add=TRUE,col=2)

#' Representamos un gráfico probabilístico normal de los valores de y 
qqnorm(y)

#' Representamos un gráfico de cajas de los valores de y
boxplot(y)

#' Representamos un gráfico de cajas múltiples (2 cajas) que permita comparar la distribución
#' x e y
boxplot(x, y, main = "Comparación de las distribuciones x e y")

#' Representamos un diagrama de dispersión de los valores de x (en el eje horizontal) frente
#' a los valores de y (en el eje vertical).
plot(x, y, main = "Diagrama de dispersión")

#' Ajustamos un modelo de regresión lineal con los datos almacenados en x e y.
mod <- lm(y ~ 1 + x)

#' Representamos el modelo ajustado sobre el diagrama de dispersión anterior
plot(x, y, main = "Diagrama de dispersión")
abline(mod)

#' Borramos memoria

rm(list=ls(all=TRUE))




#' --Ejercicio 2-------------------------------------------------------------------------

#' Escribir una función que simule el lanzamiento de n dados
#' * La función tendrá un único argumento, el número de lanzamientos n, y tomará el valor 
#'   4 por defecto. 
#' * La función devolverá como posibles valores: TRUE, si se obtiene al menos un 6
#'                                               FALSE, en caso contrario

lanzamiento.dados<-function(n=4)
{
    x<-sample(1:6,n,replace=FALSE)

    print(x)

    return(any(x == 6)) 
}

#' Utilizamos la función anterior para simular nsim=1000 jugadas de este juego, y calcular
#' la proporción de veces que se gana la apuesta de "obtener al menos un 6 en n=4 lanzamientos"
nsim<-1000
x<-sapply(rep(4,nsim), lanzamiento.dados)
mean(x)

#' Comparamos el resultado con la probabilidad exacta que es 1 - (5/6)^n
1-(5/6)^4

#' Borramos memoria

rm(list=ls(all=TRUE))



#' --Ejercicio 3-------------------------------------------------------------------------

#' Utilizando el método de inversión vamos a generar n=1000 valores de una distribución 
#' Pareto con parámetros a=5 y b=4.
n<-1000
a<-5
b<-4

set.seed(1)
u<-runif(n)

#' Evaluamos usando gráficos y el contraste de Kolmogorov-Smirnow que en efecto los valores
#' generados provienen de dicha distribución.

inversa.pareto<-function(u, a, b) {
  (b/(1-u)^(1/a))
}

x<-inversa.pareto(u,a,b)

#' Creamos el gráfico
hist(x, breaks="FD", freq=FALSE, main="Método de inversión", ylim=c(0,1.3))
lines(density(x), col="blue")

#' Evaluamos el contraste de Kolmogorov-Smirnow
f.pareto<-function(x, a, b) {
  ifelse(x >= b, (a*b^a)/(x^(a+1)), 0)
}

F.pareto<-function(x, a, b) {
  ifelse(x >= b, 1-(b/x)^a, 0)
}

ks.test(x,F.pareto,a=a,b=b)

#' Borramos memoria
rm(list=ls(all=TRUE))



#' --Ejercicio 4-------------------------------------------------------------------------

#' Utilizando la intregración de Monte Carlo se pide aproximar la integral y calcular el error
#' de aproximación.
#' Para ello consideramos un número suficientemene grande de simulaciones, evaluando la 
#' convergencia mediante un gráfico.

#' Definimos la función a integrar
h<-function(x) {
  1/(1+x^2)
}

#' Visualizamos la función en el dominio de integración
curve(h,0,1)

#' Fijamos el número de simulaciones
nsim<-1000

#' Calculamos la aproximación
set.seed(1)
x<-runif(nsim)
hx<-sapply(x,h)
mean(h)

#' Valor exacto
pi/4

#' Aproximaciones para $n=1,...,nsim$
estim<-cumsum(hx)/(1:nsim)

#' Errores de estimación correspondientes
estim.err<-sqrt(cumsum((hx-estim)^2))/(1:nsim)

plot(1:nsim,estim,type='l',ylab='Aproximación y límites de error', xlab='Número de simulaciones')
z<-qnorm(0.025,lower.tail = FALSE)
lines(estim - z*estim.err,col='blue',lwd=2,lty=3)
lines(estim + z*estim.err,col='blue',lwd=2,lty=3)
abline(h=pi/4,col=2)

#' Aproximación final y su error
estim[nsim]
estim.err[nsim]


#' Borramos memoria

rm(list=ls(all=TRUE))