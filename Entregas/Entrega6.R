# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 6---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Repetimos el proceso anterior para encontrar una raiz de f(x)=x^3-2x-5
# en el intervalo (-5,5).

# Representamos la funcion en dicho intervalo

f<-function(x) x^3-2*x-5
f.prima<-function(x) 3*x^2 -2

# Valor inicial
x0<-(-5)

# Primera iteracion
x1<-x0-f(x0)/f.prima(x0)
x1

# Segunda iteracion
x2<-x1-f(x1)/f.prima(x1)
x2

# Tercera iteracion
x3<-x2-f(x2)/f.prima(x2)
x3

# Cuarta iteracion
x4<-x3-f(x3)/f.prima(x3)
x4


# Si empezamos por el valor -5, vemos que en la cuarta iteracion todavia varian
# mucho de un resultado a otro

# En cambio si empezamos por 2, al estar cercano a la verdadera raiz vemos
# que converge rapido

# Valor inicial
x0<-(2)

# Primera iteracion
x1<-x0-f(x0)/f.prima(x0)
x1

# Segunda iteracion
x2<-x1-f(x1)/f.prima(x1)
x2

# Tercera iteracion
x3<-x2-f(x2)/f.prima(x2)
x3

# Cuarta iteracion
x4<-x3-f(x3)/f.prima(x3)
x4

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Escribimos una funcion con nombre algoritmo.NR que implemente el metodo
# de Newton-Raphson 

algoritmo.NR<-function(f,f.prima,x0,tol,nmax)
{
    it<-1
    xn<-x0
    xn_1<-xn-f(xn)/f.prima(xn)
    while (abs(xn_1-xn) >= tol && it < nmax){
        xn<-xn_1
        xn_1<-xn-f(xn)/f.prima(xn)

        it<-it+1

        if(it == nmax)  warning("Numero maximo de iteraciones alcanzado.")
    }

    return(list(xn_1,abs(xn_1-xn),it))
}

# Comprobamos el resultado de la funcion con las dos funciones que hemos explorado
# f(x) = x^2-5 y f(x)=x^3-2x-5

f<-function(x) x^2-5
f.prima<-function(x) 2*x

# Valor inicial
x0<-(2)

# Probamos la funcion
algoritmo.NR(f,f.prima,x0,0.0001,30)


# Segunda funcion 

f<-function(x) x^3-2*x-5
f.prima<-function(x) 3*x^2 -2

# Valor inicial
x0<-(2)

# Probamos la funcion
algoritmo.NR(f,f.prima,x0,0.0001,30)


# Añadimos filtros en la definicion de la funcion para controlar posibles errores

algoritmo.NR<-function(f,f.prima,x0,tol,nmax)
{
    if (missing(x0) || !is.numeric(x0)) stop("Debe proporcionar un argumento 'x0' numérico")
    else if (missing(tol) || !is.numeric(tol)) stop("Debe proporcionar un argumento 'tol' numérico")
    else if (missing(nmax) || !is.numeric(nmax)) stop("Debe proporcionar un argumento 'nmax' numérico")
    else if (missing(f) || !is.function(f)) stop("Debe proporcionar un argumento 'f' tipo funcion")
    else if (missing(f.prima) || !is.function(f.prima)) stop("Debe proporcionar un argumento 'f.prima' tipo funcion")
    
    it<-1
    xn<-x0
    xn_1<-xn-f(xn)/f.prima(xn)
    
    while (abs(xn_1-xn) >= tol && it < nmax){
        xn<-xn_1
        xn_1<-xn-f(xn)/f.prima(xn)

        it<-it+1

        if(it == nmax)  warning("Numero maximo de iteraciones alcanzado.")
    }

    return(list(xn_1,abs(xn_1-xn),it))
}

# Ejemplos
algoritmo.NR(f,8,0.0001,8,9)

algoritmo.NR(f,8,0.0001,8)



# Incluimos un sexto argumento, dibuja, de tipo logico con valor por defecto TRUE

algoritmo.NR<-function(f,f.prima,x0,tol,nmax,dibuja=TRUE)
{
    if (missing(x0) || !is.numeric(x0)) stop("Debe proporcionar un argumento 'x0' numérico")
    else if (missing(tol) || !is.numeric(tol)) stop("Debe proporcionar un argumento 'tol' numérico")
    else if (missing(nmax) || !is.numeric(nmax)) stop("Debe proporcionar un argumento 'nmax' numérico")
    else if (missing(f) || !is.function(f)) stop("Debe proporcionar un argumento 'f' tipo funcion")
    else if (missing(f.prima) || !is.function(f.prima)) stop("Debe proporcionar un argumento 'f.prima' tipo funcion")
    
    if (dibuja){
        curve(f,x0-3,x0+3)
        abline(h=0,col=2)
    }

    it<-1
    xn<-x0
    xn_1<-xn-f(xn)/f.prima(xn)
    
    while (abs(xn_1-xn) >= tol && it < nmax){
        xn<-xn_1
        xn_1<-xn-f(xn)/f.prima(xn)

        it<-it+1

        if(it == nmax)  warning("Numero maximo de iteraciones alcanzado.")
    }

    return(list(xn_1,abs(xn_1-xn),it))
}

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Instalamos el paquete numDeriv
chooseCRANmirror(graphics=FALSE)

# Seleccionamos el de Madrid (64)

install.packages('numDeriv')

# Cargamos el paquete

library(numDeriv)

# Creamos una nueva version de la funcion algoritmo.NR que utilice la aproximacion
# numerica de la derivada anterior cuando no se proporcione la expresion de la
# derivada, esto es, el argumento f.prima esta missing

algoritmo.NR<-function(f,f.prima,x0,tol,nmax,dibuja=TRUE)
{
    if (missing(x0) || !is.numeric(x0)) stop("Debe proporcionar un argumento 'x0' numérico")
    else if (missing(tol) || !is.numeric(tol)) stop("Debe proporcionar un argumento 'tol' numérico")
    else if (missing(nmax) || !is.numeric(nmax)) stop("Debe proporcionar un argumento 'nmax' numérico")
    else if (missing(f) || !is.function(f)) stop("Debe proporcionar un argumento 'f' tipo funcion")
    
    if (dibuja){
        curve(f,x0-3,x0+3)
        abline(h=0,col=2)
    }

    if (missing(f.prima) || !is.function(f.prima)){
        f.prima<-function(x) genD(func=f,x=x0)$D[1]
    }

    it<-1
    xn<-x0
    xn_1<-xn-f(xn)/f.prima(xn)
    
    while (abs(xn_1-xn) >= tol && it < nmax){
        xn<-xn_1
        xn_1<-xn-f(xn)/f.prima(xn)

        it<-it+1

        if(it == nmax)  warning("Numero maximo de iteraciones alcanzado.")
    }

    return(list(xn_1,abs(xn_1-xn),it))
}

# Comprobamos con las funciones

# Primera funcion
f<-function(x) x^2-5

# Valor inicial
x0<-(2)

# Probamos la funcion
algoritmo.NR(f=f,x0=x0,tol=0.0001,nmax=30)


# Segunda funcion 
f<-function(x) x^3-2*x-5

# Valor inicial
x0<-(2)

# Probamos la funcion
algoritmo.NR(f=f,x0=x0,tol=0.0001,nmax=30)


# Tercera funcion 
f<-function(x) exp(2*x)-x-6

# Valor inicial
x0<-(2)

# Probamos la funcion
algoritmo.NR(f=f,x0=x0,tol=0.0001,nmax=30)


# Utilizamos la funcion uniroot para encontrar la raiz de la funcion

uniroot(f,lower=-5,upper=5)

# Borramos memoria

rm(list=ls(all=TRUE))