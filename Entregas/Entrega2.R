# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadística Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 2---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

#Ejecutamos las siguientes sentencias

A<-matrix(1:9,3,3)

# Creaamos una matriz de 3 filas por 3 columnas con los valores del 1 al 9

x<-1:3

# Creamos un vector de 3 elementos del 1 al 3

A%*%x

# Hacemos el producto de Ax, que multiplica las filas de la matriz A por el vector x

A%*%t(x)

# Sale un error porque al hacer t(x) nos transforma el vector en una matriz de dimension 1 fila por 3 columnas
# y por tanto estamos multiplicando una matriz (3x3) por una matriz (1x3), cosa que no es posible

x%*%A

# Hacemos el producto xA, que  multiplica el vector x por las columnas de la matriz A

t(x)%*%A

# En este caso no sale error, porque al hacer t(x) nos transforma el vector en una matriz (1x3)
# y por tanto al multiplicar una matriz (1x3) por una matriz (3x3) no hay error

t(x)%*%x

# En este caso tampoco sale error, porque t(x) es una matriz (1x3) que lo multiplica 
# por el vector x de 3 elementos, y nos da un unico elemento como resultado

# Borramos memoria

rm(list=ls(all=TRUE))


# --Ejercicio 2-------------------------------------------------------------------------

# Ejecutamos las siguientes sentencias

# Sistema 1

solve(2,2)

# El sistema seria de la forma 2*x = 2, con solucion x=1

# Sistema 2

A<-matrix(c(3,1,4,2),2,2)
b<-c(12,8)
solve(A,b)

# El sistema seria de la forma Ax = b, con x el vector (x,y), seria:
#   3x + 4y = 12
#    x + 2y = 8


# Sistema 3

solve(A,diag(2))

# El sistema seria de la forma Ax = diag(2), con x la matriz de primera columna (x1,x2)
# y segunda (y1,y2), seria:
#   3   4       x1  y1      1   0
#   1   2  *    x2  y2  =   0   1
#   (3x1 + 4x2, 3y1 + 4y2) = (1, 0)
#   (x1 + 2x2, y1 + 2y2) = (0,1)
# 3x1 + 4x2 = 1
# 3y1 + 4y2 = 0
# x1 + 2x2 = 0
# y1 + 2y2 = 1 


# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Creamos objetos A (matriz de coeficientes)

A<-matrix(c(10,7,8,7,7,5,6,5,8,6,10,9,7,5,9,10),4:4)


# Creamos un vector de terminos independientes b

b<-c(32,23,33,31)

# Resolvemos el sistema con solve

solve(A,b)      # La solucion es 1 1 1 1

# Perturbamos el vector b sumandole 0.05 a cada uno de sus elementos

b<-b+0.05

# Calculamos de nuevo la solucion

solve(A,b)     # La solucion es 0.4 2 0.75 1.15

# Perturbamos el vector con un incremento de 0.1

b<-b-0.05+0.1

# Calculamos de nuevo la solucion

solve(A,b)      # La solucion es -0.2 3 0.5 1.3


# Solucion aumenta (-0.6, 1, -0.25, 0.15) con cada pertubacion de 0.05



# --Ejercicio 4-------------------------------------------------------------------------

# Calculamos el numero de condicion de la matriz A del sistema anterior

kappa(A)

# Calculamos el reciproco

rcond(A)

# Comprobamos que coinciden con sus definiciones

# Numero de condicion

# Suponemos p = 2 y calculamos el maximo y minimo autovalor

max<- max(eigen(A)$values)

min<- min(eigen(A)$values)

abs(max/min)

# Calculamos el reciproco como la inversa

1/abs(max/min)

# Los valores salen distintos tanto en numero de condicion y su reciproco, cuanto mas cerca
# estuviese del 0, estaria mas cerca de ser singular y el mal condicionamiento seria mas severo

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 5-------------------------------------------------------------------------

# Creamos la matriz de regresion X

n<-5
set.seed(2)
x<-rnorm(n)
X<-cbind(1,x)

# Creamos el vector de respuesta y

y<-1+x+rnorm(n,0,0.1)

# Calculamos (X'X)^(-1)

solve(t(X)%*%X)

# Usando el resultado anterior calculamos beta a partir de la expresion (3)

B<-solve(t(X)%*%X)%*%t(X)%*%y

# Observa que los datos se han generado verificando exactamente el modelo de regresion lineal
# (1) con B=(1,1)' ¿Se parece el estimador por minimos cuadrados que has obtenido a partir
# de los datos al verdadero B del modelo?

# Si, porque los valores que he obtenido son B=(1.002586, 1.024142)'

# Representamos el modelo lineal desde el qeu se han generado los datos

curve(1+x,-3,3)

# Añadimos los datos que hemos generado

points(x,y)

# Añadimos la estimacion del modelo que hemos calculado

curve(1+x,-3,3,add=TRUE,col=2)

# Borramos memoria

rm(list=ls(all=TRUE))

# Vemos la diferencia entre B y (1,1)

abs(B-1)  # (0.04539634, 0.05272454)



#------------------------------------------------

# Repetimos el ejercicio para n=50

# Creamos la matriz de regresion X

n<-50
set.seed(2)
x<-rnorm(n)
X<-cbind(1,x)

# Creamos el vector de respuesta y

y<-1+x+rnorm(n,0,0.1)

# Calculamos (X'X)^(-1)

solve(t(X)%*%X)

# Usando el resultado anterior calculamos beta a partir de la expresion (3)

B<-solve(t(X)%*%X)%*%t(X)%*%y

# Observa que los datos se han generado verificando exactamente el modelo de regresion lineal
# (1) con B=(1,1)' ¿Se parece el estimador por minimos cuadrados que has obtenido a partir
# de los datos al verdadero B del modelo?

# Si, porque los valores que he obtenido son B=(1.002586, 1.024142)'

# Representamos el modelo lineal desde el qeu se han generado los datos

curve(1+x,-3,3)

# Añadimos los datos que hemos generado

points(x,y)

# Añadimos la estimacion del modelo que hemos calculado

curve(1+x,-3,3,add=TRUE,col=2)

# Vemos la diferencia entre B y (1,1)

abs(B-1)  # (0.012758552, 0.004264957)



#------------------------------------------------

# Repetimos el ejercicio para n=500

# Creamos la matriz de regresion X

n<-50
set.seed(2)
x<-rnorm(n)
X<-cbind(1,x)

# Creamos el vector de respuesta y

y<-1+x+rnorm(n,0,0.1)

# Calculamos (X'X)^(-1)

solve(t(X)%*%X)

# Usando el resultado anterior calculamos beta a partir de la expresion (3)

B<-solve(t(X)%*%X)%*%t(X)%*%y

# Observa que los datos se han generado verificando exactamente el modelo de regresion lineal
# (1) con B=(1,1)' ¿Se parece el estimador por minimos cuadrados que has obtenido a partir
# de los datos al verdadero B del modelo?

# Si, porque los valores que he obtenido son B=(1.002586, 1.024142)'

# Representamos el modelo lineal desde el qeu se han generado los datos

curve(1+x,-3,3)

# Añadimos los datos que hemos generado

points(x,y)

# Añadimos la estimacion del modelo que hemos calculado

curve(1+x,-3,3,add=TRUE,col=2)

# Vemos la diferencia entre B y (1,1)

abs(B-1)  # (0.006454604, 0.003632366)

#------------------------------------------------

# Conclusiones

# Se observa que a medida que n aumenta, la diferencia entre B estimado y el B verdadero
# disminuye.

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 6-------------------------------------------------------------------------

# Utilizamos la misma muestra n = 5 del apartado anterior

# Creamos la matriz de regresion X

n<-5
set.seed(2)
x<-rnorm(n)
X<-cbind(1,x)

# Creamos el vector de respuesta y

y<-1+x+rnorm(n,0,0.1)

# Calculamos la descomposicion QR de la matriz X con la funcion qr

QR<-qr(X)

# Extraemos la matriz Q

Q<-qr.Q(QR)

# Calculamos el vector Q'y

Qy<-t(Q)%*%y

# Extraemos la matriz R

R<-qr.R(QR)

# Resolvemos el sistema (4) usando backsolve

B<-backsolve(R,Qy)

abs(B-1)      # (0.04539634, 0.05272454)

# En este caso los valores diferen mas del valor verdadero


# --Ejercicio propuesto----------------------------------------------------------------

# Consideramos el sistema Ax = b
# Para n=3, tenemos que A es una matriz cuadrada de dimension n cuya primera columna es
# 1,2,...n, la segunda 1^2,...,n^2, hasta la ultima 1^n,...,n^n

n<-3
A<-matrix(c((1:n)^rep(1:n,each=n)),n,n)

# Creamos un vector b como resultado del producto de A por un vector de n unos

b<-A%*%rep(1,n)

# Resolvemos el sistema usando la funcion solve

x<-solve(A,b)

# Observamos que tal y como hemos definido el sistema la solucion x es un vector d n unos
# Calculamos el maximo de las diferencias x-1 en valor absoluto

max(abs(x-1))

# Repetimos el ejercicio para n=4,5,...12

# Vamos a crear una funcion para facilitar el problema de repetirlo

ejercicio_propuesto <- function(n) {

    # Para n, tenemos que A es una matriz cuadrada de dimension n cuya primera columna es
    # 1,2,...n, la segunda 1^2,...,n^2, hasta la ultima 1^n,...,n^n

    A<-matrix(c((1:n)^rep(1:n,each=n)),n,n)

    b<-A%*%rep(1,n)
   
    x<-solve(A,b)
    
    max(abs(x-1))
}


ejercicio_propuesto(4:12)

# Según aumenta el valor de n los valores son pequeños pero mayores que 0, y en el caso
# de n=12 sale "sistema es computacionalmente singular: número de condición recíproco = 4.34177e-17"
