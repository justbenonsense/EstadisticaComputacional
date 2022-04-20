# Carlota Valdivia Manzano

# ENTREGA 1

# Ejercicio 1

# Creamos un vector x que contenga una secuencia de numeros reales entre 1 y 10 con incrementos de 0.2

x<-seq(1,10,by=0.2)

# Calculamos la longitud y la guardamos en un objeto n

n<-length(x)

# Damos nombre a cada uno de los elementos del vector del tipo x_1,...,x_n

names(x)<-paste(c("x"),1:n,sep="_")

# Calculamos la media de x y la almacenamos en un objeto mx

mx<-mean(x)

# Calculamos cuantos elementos de x estan por encima de mx

length(x[x>mx])

# Calculamos la posicion que ocupa el elemento de x mas proximo por encima de mx

which(x==min(x[x>mx]))

# Creamos otro vector y con los primeros n numeros impares

y<-seq(1,n*2,by=2)

# Imprimimos los elementos de x que ocupen las posiciones indicadas por los primeros 5 elementos de y

x[y[1:5]]

# Borramos memoria

rm(list=ls(all=TRUE))



# Ejercicio 2

# Evaluamos la funcion en una rejilla de valores equiespaciados en el intervalo [-2,2] con incremento 0.1

x<-seq(-2,2,by=0.1)

(x<(-1))+((-1)<=x & x<0)*(log(x^2))+(0<=x & x<1)*(log(x^2+1))+(x>=1)*2

# Borramos memoria

rm(list=ls(all=TRUE))



# Ejercicio 3

# Fijamos la semilla de generacion de numeros aleatorios

set.seed(1)

# Creamos un vector con nombre x que contiene 50 valores aleatorios de una distr unif en el intervalo unidad usando runif

x<-runif(50)

# Calculamos cuantos de sus elementos estan en el intervalo (0.25,0.75)

length(x[x>0.25 & x<0.75])

# Calculamos cuantos de sus elementos estan por dejado de 0.1 o por encima de 0.9

length(x[x<0.1 | x>0.9])

# Reemplazamos dichos elemento por el valor NA

x[x<0.1 | x>0.9]<-NA

# Calculamos su media

mean(x,na.rm=TRUE)

# Reemplazamos los valores NA por ceros

x[is.na(x)]<-0

# Calculamos su media 

mean(x)

# La media de ahora es menor que la anterior

# Borramos memoria

rm(list=ls(all=TRUE))



# Ejercicio 4

# Creamos un vector con los 20 primeros terminos de la progresion aritmetica a_n=a_1+(n-1)d
# con a_1=1 y d=1.2

n<-20
a_1<-1
d<-1.2
a<-a_1 + ((1:n)-1)*d

# Calculamos la suma de sus elementos usando la funcion sum

sum(a)

# Comprobamos que coincide con la formula n(a_1+a_n)/2 para n=20

n*(a[1]+a[n])/2

# Ambos dan el resultado 248

sum(a) == n*(a[1]+a[n])/2

# Calculamos la cuasi-desviacion tipica usando la funcion sd

sd(a)

# Comprobamos que coincide con |d|sqrt((n(n+1))/12)

abs(d)*sqrt((n*(n+1))/12)

sd(a) == abs(d)*sqrt((n*(n+1))/12)

# Calculamos el producto de sus elementos usando la fucnion prod 

prod(a)

# Comprobamos que coincide con su respectiva f�rmula

d^n*(gamma(a[1]/d+n)/gamma(a[1]/d))

prod(a) == d^n*(gamma(a[1]/d+n)/gamma(a[1]/d))

# Borramos memoria

rm(list=ls(all=TRUE))


# Ejercicio 5

# Creamos un vector x con elementos 2,2,8,7,6,1 y 5

x<-c(2,2,8:6,1,5)
x

# Calculamos en una unica sentencia las diferencias sucesivas de sus elementos

x[1:(length(x)-1)]-x[2:length(x)]

# Vemos que coincide con los resultados de diff

diff(x)

# Borramos memoria

rm(list=ls(all=TRUE))


# Ejercicio 6

# Creamos un vector con nombre ABE con las las letras del abecedario en mayuscula

ABE<-LETTERS

# Seleccionamos aleatoriamente 5 letras y las almacenamos en un vector on nombre ABE.5

ABE.5<-sample(ABE,5,replace=FALSE)

# Creamos un vector con nombre PAL con 2 elementos consistentes en 2 palabras formadas 
# colocando aleatoriamente las 5 letras anteriores sin repeticiones.
PAL<-c()
PAL[1]<-paste(sample(ABE.5,replace=FALSE),sep="",collapse="")
PAL[2]<-paste(sample(ABE.5,replace=FALSE),sep="",collapse="")



