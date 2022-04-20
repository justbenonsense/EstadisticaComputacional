# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 3---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Creamos un objeto de tipo lista con tres componentes: x1,x2 y x3

x1<-1:5
x2<-2:6
x3<-3:7
lista<-list(x1,x2,x3)

lista<-list(x1=1:5,x2=2:6,x3=3:7)

# Creamos un vector x con una muestra de 10 num aleatorios de una distribucion
# uniforme en el intervalo (0,1)

# Fijamos la semilla de generacion de numeros aleatorios

set.seed(1)


# Creamos un vector con nombre x que contiene 10 valores aleatorios de una distr unif en el intervalo unidad usando runif

x<-runif(10)

# Añadimos dicho vector a la lista

lista[['x']]<-x

# Creamos un vector y con una muestra de 10 num aleatorios con una distribucion
# normal estandar.

y<-rnorm(10)

# Añadimos dicho vector a la lista

lista[['y']]<-y

# Utilizamos la función lapply para calcular la suma de cada componente de la lista

lapply(lista,sum)

# Vemos que devuelve una lista

# Utilizamos ahora sapply

sapply(lista,sum)

# Vemos que en  este caso ha devuelto un tipo matriz

# Escribimos el siguiente codigo

reg<-lm(y~x)

# Utilizamos una funcion adecuada para confirmar que reg es un objeto tipo lista

typeof(reg)
is.list(reg)

# Nos sale que es tipo list

# Utilizamos la funcion adecuada para obtener que tipo de objeto constituyen
# las componentes de reg

lapply(reg,typeof)
sapply(reg,typeof)

# Creamos un matriz que contenga por columnas las componentes residuals y fitted.values del
# objeto reg, ademas de los vectores x e y

A<-cbind(reg[['residuals']],reg[['fitted.values']],x,y)

# Añadimos nombres a las columnas de dicha matriz

colnames(A)<-c('residuals','fitted.values','x','y')

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Creamos un data frame con nombre datos

xi<-c(1.2,1.8,2.2,2.5,1.1)
yi<-c(15,18,10,12,16)
ni<-c(12,23,5,9,11)

datos<-data.frame(xi,yi,ni)

# Calculamos el tamaño de la muestra

n<-sum(datos[3])

# Calculamos las medias aritmeticas de las observaciones de las variables x e y

x_med<-sum(datos[1]*datos[3])/n
y_med<-sum(datos[2]*datos[3])/n

# Calculamos las cuasivarianzas

sum((datos[1]-x_med)^2*datos[3])/(n-1)
sum((datos[2]-y_med)^2*datos[3])/(n-1)

# Cramos un data frame con nombre datos.n que recoja las n observaciones individuales por filas
# Repitiendo las filas de "datos" tantas veces como indique la columna de la frecuencia absoluta

datos.n<-data.frame(rep(xi,ni),rep(yi,ni))

# A partir del data frame datos.n calculamos de nuevo las medias aritmeticas y cuasivarianzas

x_mean<-colMeans(datos.n[1])
y_mean<-colMeans(datos.n[2])

x_var<-var(datos.n[1])
y_var<-var(datos.n[2])

# Añadimosdos columnas al final del data frame datos.n con los valores tipificados de las
# variables x e y

# Usando la funcion transform

transform(datos.n,zi=(datos.n[1]-x_mean)/c(x_var))

# Usando la funcion within

within(datos.n,{zi=(datos.n[1]-x_mean)/c(x_var)})


# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Imprimimos en la consola las primeras 5 filas del dataframe ChickWeight 
head(ChickWeight,5)

# Imprimimos en la consola las ultimas 3 filas del dataframe ChickWeight 
tail(ChickWeight,3)

# Imprimimos la estructura del objeto ChickWeight

str(ChickWeight)

# Realizamos un resumen descriptivo numero elemental de todas las variables del
# data frame con summary

summary(ChickWeight)

# Realizamos el mismo tipo de resumen pero ahora solo de la variable weight para los
# distintos niveles del factor dieta, usando la funcion tapply
# Almacenamos el resultado en un objeto con nombre peso.dieta

peso.dieta<-tapply(ChickWeight$weight,ChickWeight$Diet,summary)

# Vemos el tipo de objeto
typeof(peso.dieta)


# Creamos un data frame peso.dieta.2 colocando por columnas el resume obtenido del 
# peso para cada tipo de dieta
# Cada columna tendra el nombre de la correspondiente medida descriptiva

peso.dieta.2<-data.frame(matrix(unlist(peso.dieta),nrow=length(peso.dieta),byrow=TRUE))
colnames(peso.dieta.2)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")


# Utilizamos la funcion aggregate para realizar el mismo resumen que antes en el objeto
# peso.dieta
aggregate(ChickWeight$weight,list(ChickWeight$Diet),summary)


# Vemos el tipo
typeof(aggregate(ChickWeight$weight,list(ChickWeight$Diet),summary))


# Volvemos a crear el data frame peso.dieta.2 con la estructura especificada antes a
# partir del objeto que devuelve aggregate
peso.dieta.3<-data.frame(aggregate(ChickWeight$weight,list(ChickWeight$Diet),summary)[2])
colnames(peso.dieta.3)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")


# Creamos un data frame Chick100 con una submuestra de los datos contenidos en ChickWeight
# seleccionando aleatoriamente (sin reemplazo) 100 filas.
Chick100<-ChickWeight[sample(nrow(ChickWeight),100), ]

# Muestra el data frame Chick100 con sus columnas permutadas aleatoriamente
Chick100[sample(nrow(Chick100),100), ]

# Muestra el data frame Chick100 con sus columnas por orden alfabetico
Chick10[order(names(Chick10))]


# Borramos memoria

rm(list=ls(all=TRUE))