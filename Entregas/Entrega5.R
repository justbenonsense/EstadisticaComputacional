# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 5---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Guardamos en un .txt los datos que queremos almacenar

muestra<-scan(file="datos5.txt")

# Otra forma

muestra<-scan()
25.03 18.59 47.20 80.20 187.67
95.94 35.07 145.38 9.52 128.14
136.69 180.82 49.67 33.41 4.16
94.87 102.25 11.04 35.14 151.15
17.14 81.94 20.01 125.26 7.11
61.36 55.59 10.80 31.88 16.39
45.95 4.98 23.20 8.78 30.68
22.65 13.19 40.62 2.78 35.41
8.63 17.04 8.02 126.54 2.11
136.93 17.39 37.73 84.53 14.22


# --Método 1------------------------------------------------

# Definimos la función que calcule la log-verosimilitud

logl<-function(theta)
{
    a<-theta[1]
    b<-theta[2]
    l<-sum(log(dgamma(x=muestra,shape=a,scale=b)))
    return(-l)
}

# Establecemos los valores iniciales para los parámetros
b0<-a0<-1

# Usamos la función optimm para optimizar la función logl con los parámetros a0 y b0
res<-optim(par=c(a0,b0),fn=logl)

# Vemos que se almacena el óptimo en la componente res$par
res$par

# Vemos que la lista res contiene un elemento que es el $par donde se almacenan
# los óptimos de la función logl, después tenemos $value y $counts 
# Por otro lado tenemos $converge lo cual nos dice que la función converge rápido

# Establecemos los valores iniciales para los parámetros
b0<-a0<-2

# Usamos la función optimm para optimizar la función logl con los parámetros a0 y b0
res<-optim(par=c(a0,b0),fn=logl)

# Vemos que se almacena el óptimo en la componente res$par
res$par


# Instalamos el paquete Rsolnp

chooseCRANmirror(graphics=FALSE)
# Seleccionamos el de Madrid (64)

install.packages('Rsolnp')

# Cargamos el paquete
library(Rsolnp)

# Establecemos los valores iniciales para los parámetros
b0<-a0<-1

# Usamos la función optimm para optimizar la función logl con los parámetros a0 y b0
res<-solnp(pars=c(a0,b0),fun=logl,LB=c(0,0))

# Vemos que se almacena el óptimo en la componente res$par
res$pars

# Establecemos los valores iniciales para los parámetros
a0<-var(muestra)/mean(muestra)
b0<-mean(muestra)/a0

# Usamos la función optimm para optimizar la función logl con los parámetros a0 y b0
res<-solnp(pars=c(a0,b0),fun=logl,LB=c(0,0))

# Vemos que se almacena el óptimo en la componente res$par
res$pars


# Instalamos el paquete Rsolnp

chooseCRANmirror(graphics=FALSE)
# Seleccionamos el de Madrid (64)

install.packages('maxLik')

# Cargamos el paquete
library(maxLik)


logl2<-function(theta) -logl(theta)
res<-maxLik(logl2,start=c(1,1))
res

# Observamos que el máximo se almacena en la componente estimate
res$estimate

# Cambiamos los valores iniciales

res<-maxLik(logl2,start=c(a0,b0))
res
res$estimate

# Si se producen cambiamos en los resultados

# Introducimos las siguientes restricciones

A<-matrix(c(1,0,0,1),2)
B<-c(0,0)
maxLik(logl2,start=c(1,1),constraints=list(ineqA=A,ineqB=B))

# Los resultados son similares pero sin mensajes de advertencia



# --Método 2------------------------------------------------

f<-function(a) log(a)-digamma(a)- log(mean(muestra))+mean(log(muestra))
res<-uniroot(f,c(0.1,100))
res

# Calculamos b a partir de a
b<-mean(muestra)/res$root

# Los resultados son similares 


# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Construimos una funcion con nombre medias que nos devuelva una lista con tres
# componentes: media aritmetica, media armonica, media geometrica

medias<-function(x)
{
    if( is.numeric(x) ){
        y<-x[!is.na(x)]
        n<-length(y)
        media_arit<-sum(y)/n

        if ( min(x) > 0 ){
            media_geo<-(prod(y))^(1/n)
            media_arm<-n/(sum(1/y))

            return(c(media_arit,media_arm,media_geo))
        }
        else {
            warning("El vector tiene valores negativos o 0")

            return(c(media_arit))
        }
    }
    else warning("El vector debe ser numérico")
}

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# 

# Borramos memoria

rm(list=ls(all=TRUE))