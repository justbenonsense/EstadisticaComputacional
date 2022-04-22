# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# -------------------------------PARCIAL 1---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Creamos un vector x con los primeros 50 números impares
x<-seq(1,by=2,length=50)

# Calculamos la media de sus elementos,mx, y la cuasidesviacion tipiica, sx
mx<-mean(x)
sx<-sd(x)

# Calculamos el número de elementos en x que disten de mx más de sx unidades
sum(abs(x-mx)<sx)

# Sustituimos los elementos en x que hemos localizado en el apartado anterior 
# por valores perdidos
x[abs(x-mx)<sx]<-NA

# Contamos cuantos multiplos de 3 hay en el vector x resultante
sum(x%%3==0,na.rm=TRUE)

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Creamos una copia del conjunto de datos en el espacio de trabajo con nombre
# aire, e imprimimos la estructura del objeto de datos

aire<-airquality; aire

# Contamos el numero de datos perdidos que hay en cada columna del objeto aire
lapply(lapply(aire,is.na),sum)

# Cuenta el numero de filas que vamos a eliminar
sum(complete.cases(aire)==FALSE)

# Elimina todas las filas del objeto aire que contienen algun dato perdido
aire<-na.omit(aire)

# Convertimos la columna Month en un factor y asignamos como nombre a sus niveles 
# el mes correspondiente
aire$Month<-factor(aire$Month,labels=month.name[5:9])

# Calculamos las medianas de las columns Wind y Ozone para cada uno de los meses
# considerdos

mediana_wind<-tapply(aire$Wind,aire$Month,median)
mediana_ozone<-tapply(aire$Ozone,aire$Month,median)

# Creamos un data frame, aire.mayo, con los dtos de aire correspodientes al mes
# de mayo

aire.mayo<-subset(aire,subset= Month=="May")

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Creamos una funcion en R que permita calcular los n primero terminos de la prog
# geometrica

prog.geometrica<-function(n,a1,r)
{
    if (missing(n) || !is.numeric(n)) stop("Debe proporcionar un argumento 'n' numérico")
    else if (missing(a1) || !is.numeric(a1)) stop("Debe proporcionar un argumento 'a1' numérico")
    else if (missing(r) || !is.numeric(r)) stop("Debe proporcionar un argumento 'r' numérico")

    v<-a1*r^((1:n)-1)
    suma1<-sum(v)
    suma2<-(v[1]*(1-r^n)/(1-r))
    producto1<-prod(v)

    if(a1>0 & r>0){
        producto2<-(sqrt((a1^2)*r^(n-1)))^n
    }
    else {
        producto2<-NA
    }
    
    return(list(v,suma1,suma2,producto1,producto2))
}

prog.geometrica(20,2,-0.5)
prog.geometrica(20,2,0.5)

# Borramos memoria

rm(list=ls(all=TRUE))