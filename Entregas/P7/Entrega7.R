# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 7---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Cargamos los datos en R del fichero Employee.txt
employee<-read.table(file="Employee.txt",header=TRUE, as.is=c(1))

# Renombramos los niveles del factor gender como female y male
employee$gender<-factor(employee$gender,labels=c("female","male"))

# Simplicamos el código 
attach(employee)

# -----------------------------------------------------------------
# --Analisis de Salary-------------------------------------------
# -----------------------------------------------------------------

# Analizamos la variable salary con un histograma
hist(salary)

# Guardamos los valores usamos en el gráfico 
res<-hist(salary,plot=FALSE); res

# Incrementamos el número de intervalos del histograma a 100
hist(salary,breaks=100)

# Definimos intervalos de distinta amplitud
x1 <-seq(15000,40000,by=5000)
x2 <-seq(50000,80000,by=10000)
x3 <-seq(100000,140000,by=20000)
hist(salary,breaks=c(x1,x2,x3))

# Vemos una versión suavidaza del estimador usando la función density
lines(density(salary),col="blue")

# Calculamos la media y la desviación de salary
mx<-mean(salary)
sx<-sd(salary)

# Sobre el gráfico anterior dibujamos la función de densidad de una Normal cuya media
# y desviación tipica sean las de los datos de salary
curve(dnorm(x,mean=mx,sd=sx), col=3, add=TRUE)

# Obtenemos un grafico probabilistico normal
qqnorm(salary)

# Obtenemos los contrastes de Kolmogorov-Smirnov y Shapiro-Wilks
ks.test(salary,pnorm,mean=mean(salary),sd=sd(salary))
shapiro.test(salary)

# Hacemos un diagrama de caja
boxplot(salary)

# Obtenemos un resumen de los datos
summary(salary)

# Construimos un grafico que superpone histograma, densidad suavizada y grafico de cajas
hist(salary,probability=TRUE,main="",axes=FALSE)
axis(1)
lines(density(salary),col='red',lwd=2)
## Para que el próximo gráfico se superponga al anterior
par(new=TRUE) 
boxplot(salary,horizontal=TRUE, axes=FALSE,lwd=2)

# Usamos el gráfico de cja para comparar la distribución entre hombres y mujeres
boxplot(salary~gender)



# -----------------------------------------------------------------
# --Analisis de Startsal-------------------------------------------
# -----------------------------------------------------------------

# Analizamos la variable startsal con un histograma
hist(startsal)

# Guardamos los valores usamos en el gráfico 
res<-hist(startsal,plot=FALSE); res

# Incrementamos el número de intervalos del histograma a 100
hist(startsal,breaks=100)

# Definimos intervalos de distinta amplitud
x1 <-seq(15000,40000,by=5000)
x2 <-seq(50000,80000,by=10000)
x3 <-seq(100000,140000,by=20000)
hist(startsal,breaks=c(x1,x2,x3))

# Vemos una versión suavidaza del estimador usando la función density
lines(density(startsal),col="blue")

# Calculamos la media y la desviación de startsal
mx<-mean(startsal)
sx<-sd(startsal)

# Sobre el gráfico anterior dibujamos la función de densidad de una Normal cuya media
# y desviación tipica sean las de los datos de startsal
curve(dnorm(x,mean=mx,sd=sx), col=3, add=TRUE)

# Obtenemos un grafico probabilistico normal
qqnorm(startsal)

# Obtenemos los contrastes de Kolmogorov-Smirnov y Shapiro-Wilks
ks.test(startsal,pnorm,mean=mean(startsal),sd=sd(startsal))
shapiro.test(startsal)

# Hacemos un diagrama de caja
boxplot(startsal)

# Obtenemos un resumen de los datos
summary(startsal)

# Construimos un grafico que superpone histograma, densidad suavizada y grafico de cajas
hist(startsal,probability=TRUE,main="",axes=FALSE)
axis(1)
lines(density(startsal),col='red',lwd=2)
## Para que el próximo gráfico se superponga al anterior
par(new=TRUE) 
boxplot(startsal,horizontal=TRUE, axes=FALSE,lwd=2)

# Usamos el gráfico de caja para comparar la distribución entre hombres y mujeres
boxplot(startsal~gender)



# -----------------------------------------------------------------
# --Analisis de Age------------------------------------------------
# -----------------------------------------------------------------

# Analizamos la variable age con un histograma
hist(age)

# Guardamos los valores usamos en el gráfico 
res<-hist(age,plot=FALSE); res

# Incrementamos el número de intervalos del histograma a 100
hist(age,breaks=100)

# Definimos intervalos de distinta amplitud
x1 <-seq(15000,40000,by=5000)
x2 <-seq(50000,80000,by=10000)
x3 <-seq(100000,140000,by=20000)
hist(age,breaks=c(x1,x2,x3))

# Vemos una versión suavidaza del estimador usando la función density
lines(density(staagertsal),col="blue")

# Calculamos la media y la desviación de age
mx<-mean(age)
sx<-sd(age)

# Sobre el gráfico anterior dibujamos la función de densidad de una Normal cuya media
# y desviación tipica sean las de los datos de age
curve(dnorm(x,mean=mx,sd=sx), col=3, add=TRUE)

# Obtenemos un grafico probabilistico normal
qqnorm(age)

# Obtenemos los contrastes de Kolmogorov-Smirnov y Shapiro-Wilks
ks.test(age,pnorm,mean=mean(age),sd=sd(age))
shapiro.test(age)

# Hacemos un diagrama de caja
boxplot(age)

# Obtenemos un resumen de los datos
summary(age)

# Construimos un grafico que superpone histograma, densidad suavizada y grafico de cajas
hist(age,probability=TRUE,main="",axes=FALSE)
axis(1)
lines(density(age),col='red',lwd=2)
## Para que el próximo gráfico se superponga al anterior
par(new=TRUE) 
boxplot(age,horizontal=TRUE, axes=FALSE,lwd=2)

# Usamos el gráfico de caja para comparar la distribución entre hombres y mujeres
boxplot(age~gender)


# -----------------------------------------------------------------
# --Analisis conjunto de dos variables: salary y startsal----------
# -----------------------------------------------------------------

# Para visualizar la relacion usamos la funcion plot
plot(startsal,salary)

#  El grafico anterior nos muestra que parece existir una fuerte relacion entre 
# ambas variables. Esta se puede describir a traves de un modelo de regresion
# lineal simple, cuyo ajuste lo podemos obtener con la funcion lm
# y superponerlo usando la funcion abline
mod<-lm(salary~startsal)

plot(startsal,salary)
abline(mod,col='blue')

# -----------------------------------------------------------------
# --Analisis conjunto de dos variables: salary y age---------------
# -----------------------------------------------------------------

# Para visualizar la relacion usamos la funcion plot
plot(age,salary)

#  La relacion entre ambas variables se puede describir a traves de un modelo 
# de regresion lineal simple, cuyo ajuste lo podemos obtener con la funcion lm
# y superponerlo usando la funcion abline
mod<-lm(salary~age)

plot(age,salary)
abline(mod,col='blue')

# En este caso la relación entre edad y salario no es tan fuerte como en el
# caso anterior

# -----------------------------------------------------------------
# --Analisis conjunto de dos variables: salary y edu---------------
# -----------------------------------------------------------------

# Para visualizar la relacion usamos la funcion plot
plot(edu,salary)

#  La relacion entre ambas variables se puede describir a traves de un modelo 
# de regresion lineal simple, cuyo ajuste lo podemos obtener con la funcion lm
# y superponerlo usando la funcion abline
mod<-lm(salary~edu)

plot(edu,salary)
abline(mod,col='blue')

# En este caso podemos observar que la relación no es tan fuerte como en el
# primer caso. 


# -----------------------------------------------------------------
# --Variables cuantitativas:jobcat---------------------------------
# -----------------------------------------------------------------

# Construimos tablas de frecuencias absolutas
tab<-table(jobcat)
tab

# Construimos tablas de frecuencias relativas
tab.fi<-prop.table(tab)
tab.fi

# Construimos una tabla clasica de frecuencias
data.frame(tab,Freq.rel=as.numeric(tab.fi))

# Usamos la funcion barplot para realizar un diagrama de barras
barplot(tab)

# Usamos la funcion pie para realizar un diagrama de sectores
pie(tab)

# -----------------------------------------------------------------
# --Variables cuantitativas:gender---------------------------------
# -----------------------------------------------------------------

# Construimos tablas de frecuencias absolutas
tab<-table(gender)
tab

# Construimos tablas de frecuencias relativas
tab.fi<-prop.table(tab)
tab.fi

# Construimos una tabla clasica de frecuencias
data.frame(tab,Freq.rel=as.numeric(tab.fi))

# Usamos la funcion barplot para realizar un diagrama de barras
barplot(tab)

# Usamos la funcion pie para realizar un diagrama de sectores
pie(tab)


# -----------------------------------------------------------------
# --Variables cuantitativas:minority-------------------------------
# -----------------------------------------------------------------

# Construimos tablas de frecuencias absolutas
tab<-table(minority)
tab

# Construimos tablas de frecuencias relativas
tab.fi<-prop.table(tab)
tab.fi

# Construimos una tabla clasica de frecuencias
data.frame(tab,Freq.rel=as.numeric(tab.fi))

# Usamos la funcion barplot para realizar un diagrama de barras
barplot(tab)

# Usamos la funcion pie para realizar un diagrama de sectores
pie(tab)



# -----------------------------------------------------------------
# --Analisis conjunto de dos variables cuantitativas: jobcat-salary
# -----------------------------------------------------------------

# Construimos una tabla de frecuencias cruzada
tab2<-table(jobcat,gender); tab2

# Añadimos las sumas por filas y columnas
addmargins(tab2)

# Creamos un diagrama de barras
barplot(tab2)

# Incluimos una leyenda
barplot(tab2,legend.text=TRUE, args.legend=list(x='topleft',bty='n'),
+ ylim=c(0,300), density=30,col=c('green','blue','red'),
+ main='Number of employees by gender and job category')

barplot(tab2,legend.text=TRUE, args.legend=list(x='top',bty='n'),
+ density=30,col=c('green','blue','red'),
+ main='Number of employees by gender and job category',
+ beside=TRUE)


# -----------------------------------------------------------------
# Analisis conjunto de dos variables cuantitativas: jobcat-minority
# -----------------------------------------------------------------

# Construimos una tabla de frecuencias cruzada
tab2<-table(jobcat,minority); tab2

# Añadimos las sumas por filas y columnas
addmargins(tab2)

# Creamos un diagrama de barras
barplot(tab2)

# Incluimos una leyenda
barplot(tab2,legend.text=TRUE, args.legend=list(x='topleft',bty='n'),
ylim=c(0,300), density=30,col=c('green','blue','red'),
main='Number of employees by gender and job category')

barplot(tab2,legend.text=TRUE, args.legend=list(x='top',bty='n'),
density=30,col=c('green','blue','red'),
main='Number of employees by gender and job category',
beside=TRUE)

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Simplicamos el código 
attach(airquality)

# Construimos un histograma del contaminante Ozone considerando intervalos
# de amplitud 10
x1 <-seq(0,200,by=10)
hist(Ozone,breaks=x1)

# Superponemos al histograma la funcion densidad de una distribucion Normal
# cuyos parametros media y desviacion tipica sean las de los datos Ozone

# Calculamos la media y la desviación de Ozone
mx<-mean(Ozone,na.rm=TRUE)
sx<-sd(Ozone,na.rm=TRUE)

# Sobre el gráfico anterior dibujamos la función de densidad de una Normal cuya media
# y desviación tipica sean las de los datos de Ozone
curve(dnorm(x,mean=mx,sd=sx), col=3, add=TRUE)

# Construimos un grafico de normalidad para la variable Ozone

# Borramos memoria

rm(list=ls(all=TRUE))


