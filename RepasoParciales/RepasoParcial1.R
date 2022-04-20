# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# -------------------------------REPASO PARCIAL 1---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Creamos un vector x con 100 números aleatorios desde una distribucion
# uniforme en el intervalo (0,1)
set.seed(1)
x<-runif(100)

# Calculamos la media de sus elementos (mx)
mx<-mean(x)

# Localizamos en qué posición se encuentra el elemento del vector mas cercano
# a dicha media, y los imprimimos
pos_cerc<-which.min(abs(mx-x)); pos_cerc

# Calculamos el numero de elementos en x que estan por debajo de mx
length(x[x<mx])

# o sum(x<mx)

# Eliminamos los elementos en x que hemos localizado en el apartado anterior
x<-x[-which(x<mx)]

# Creamos una matriz A con dos columnas, la primera es el vector x, y la segunda
# el vector que contenga la dist en abs de x-mx
A<-cbind(x,abs(x-mx))

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Leemos los datos en R y los almacenamos en un data fram con nombre hatco
hatco<-read.table(file="hatco.txt",header=TRUE)

# Convertimos la columna x8 en un factor asignadole los nombres a sus niveles
hatco$x8<-as.factor(c("Pequeña","Grande"))
class(hatco$x8)

# Transformamos la primera columna en un vector de tipo caracter
hatco$cliente<-as.character(hatco$cliente)
class(hatco$cliente)

# Contamos cuantas empresas hay de cada tipo e imprimimos los datos de una
# de cada tipo elegida al azar
s<-tapply(hatco$x8,hatco$x8,length)

hatco[hatco$x8=="Pequeña",][sample(s[1],1),]
hatco[hatco$x8=="Grande",][sample(s[2],1),]

# Calculamos el nivel de fidelidad medio de todas las empresas
media<-mean(hatco$y)

# Calculamos la media para empresas pequeñas y grandes
medias_sep<-tapply(hatco$y,hatco$x8,mean)

# Contamos cuantas empresas pequeñas tiene un nivel de fidelidad por encima de su media
# Idem para empresas grandes
length(hatco$y[hatco$x8=="Pequeña" & hatco$y>medias_sep[1]])
length(hatco$y[hatco$x8=="Grande" & hatco$y>medias_sep[2]])

# Creamos un data frame hatco2 con las columnas numericas de hatco tipificadas
hatco2<-scale(Filter(is.numeric,hatco))

# Ó
numeric_cols <- unlist(lapply(hatco, is.numeric))
scale(hatco[, numeric_cols])

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Creamos una funcion que permita calcular los n primeros terminos de la progresion
# aritmetica, con argumentos a1, n y r, dolviendo un objeto lista con
# - vector v con los n terminnos calculados
# - suma de sus elementos
# - producto de sus elementos

prog.aritmetica<-function(n,a1,r)
{
    an<-a1+d*(1:n)

    return(list(an,sum(an),prod(an)))
}

# Segunda version que incluya un argumento adicional (explicit) de tipo logico
# con valor FALSE por defecto.
# Cuando tome TRUE, el calculo de la suma y el producto se realizaran con la 
# siguiente expresiones

prog.aritmetica.2<-function(n,a1,r,explicit=FALSE)
{
    an<-a1+d*(1:n)

    if(explicit){
        sum<-n*(an[1]+an[n])/2
        prod<-d^n*(gamma(an[1]/d+n))/(gamma(an[1]/d))

        return(list(an,sum,prod))
    }    
    else            return(list(an,sum(an),prod(an)))
    
}

# Borramos memoria

rm(list=ls(all=TRUE))