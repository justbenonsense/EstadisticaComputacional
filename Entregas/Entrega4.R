# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 4---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Importamos los datos en R dentro de un data frame con nombre censo
# usando la función read.table o read.csv

censo<-read.csv(file="Census.csv",header=TRUE,stringsAsFactors=TRUE)
censo<-read.csv(file="Census.csv",header=TRUE,as.is=c(1,3,4,6))

# Comprobamos el tipo de datos de las columnas del data frame

str(censo)

# Observamos que en el data frame hay varios valores NA
# Contamos cuantos hay en cada columnas
lapply(lapply(censo, is.na), sum)


# Contamos cuanta filas del data frame estan completas, es decir, no tienen ningun
# datos perdido

x<-complete.cases(censo)
length(x[x==TRUE])

# Otra forma

sum(complete.cases(censo))

# Creamos un nuevo data frame con nombre censo2, copiando solamente las filas del censo
# completas

# Usando complete.cases

censo2<-censo[complete.cases(censo),]
str(censo2)

# Usando na.omit

censo2<-na.omit(censo)
str(censo2)

# Escribimos el contenido del data frame con nombre censo2 en un fichero censo2.txt
# con la funcion write.table

write.table(censo2,"censo2.txt",sep="\t",row.names=FALSE)

# Importamos los datos del fichero censo2.txt en un data frame con nombre datos3
# Tiene que coincidir en estructura y composicion con datos2

censo3<-read.table("censo2.txt",header=TRUE)
str(censo3)

# Borramos memoria

rm(list=ls(all=TRUE))




# --Ejercicio 2-------------------------------------------------------------------------

# Creamos una matriz con nombre matriz con 10 filas y 5 columnas cuyos elementos
# sean valores aleatorios de una distribucion normal

matriz<-matrix(rnorm(10*5),nrow=10,ncol=5)

# Asignamos nombres a las columnas de la matriz del tipo col1,...col5

colnames(matriz)<-c("col1","col2","col3","col4","col5")


# Imprimimos la matriz anterior en un fichero de texto matriz.txt usando write

write(t(matriz),file='matrix.txt',ncolumns=5) # no pone los nombres de las columnas

write.table(matriz,'matrix.txt',sep=",",col.names=TRUE,row.namesS=FALSe) # pone los nombres de las columnas


# Leemos el fichero que hemos escrito y almacenamos su informacion en el
# espacio de trabajo en forma de data frame
datafram<-read.table("matriz.txt",header=TRUE,sep=",")

# Borramos memoria

rm(list=ls(all=TRUE))



# --Ejercicio 3-------------------------------------------------------------------------

# Importamos los datos en un dataframe olimpics, de forma que las columnas 
# que correspondan a factores se almacenen con ese tipo

olimpics<-read.csv("Olympics100m.csv",)

# Comprobamos con una funcion adecuada si hay algun dato perdido, contando cuantos hay

lapply(lapply(olimpics, is.na), sum)

# Calculamos un resumen con summary.

summary(olimpics)

#Almacenamos el valor devuelto en un objeto resumen

resumen<-summary(olimpics)

# Comprobamos que es una matriz de tipo caracter

typeof(resumen)

# Imprimimos dicho objeto en un fichero de texto resumen.txt

write.table(resumen,"resumen.txt",col.names=TRUE,row.names=FALSE,sep=",")

resumen_check<-read.table("resumen.txt",header=TRUE,sep=",")

# Calculamos un resumen descriptivo de la variable TIME para los distintos niveles 
# del factor Gender
summary(olimpics[olimpics$Gender=="female", ])
summary(olimpics[olimpics$Gender=="male", ])
resumen2<-aggregate(olimpics$TIME,by=list(olimpics$Gender),summary)

# Imprimimos dicho data frame en resumen2.txt
write.table(resumen2,"resumen2.csv",col.names=TRUE,row.names=FALSE,sep=",")

read.csv("resumen2.csv",header=TRUE)

# Borramos memoria

rm(list=ls(all=TRUE))