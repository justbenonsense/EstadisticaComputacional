# -------------------------------------------------------------------------------------
# Carlota Valdivia Manzano
# Estadistica Computacional
# Curso 2021-2022
# --------------------------------------------------------------------------------------

# --------------------------------ENTREGA 9---------------------------------------------

# --Ejercicio 1-------------------------------------------------------------------------

# Cargamos los datos de hatco2.csv almacenandolos en un dataframe con nombre
# hatco donde las variables de tipo factor se identifiquen como tal
hatco<-read.csv(file="hatco2.csv",header=TRUE,stringsAsFactors=TRUE)

# Primero representamos graficamente los datos para observar la posible
# relacion entre la respuesta y las variables explicativas
plot(hatco[,c(6:13)])

# Observamos el gráfico y comentamos...

# Podríamos ver la relación que hay entre la fidelidad y la velocidad, y entre
# la fidelidad y servconj.
# Por otro lado, podemos observar que hay una gran relación lineal entre
# servconj e imgfvent

# Ajustamos el modelo de regresion lineal multiple a las n=100 observaciones
mod1<-lm(fidelidad~velocidad+precio+flexprec+imgfabri+servconj+imgfvent+calidadp,hatco)
mod1

# Vamos a calcular el contraste de regresion y los coeficientes de determinacion 
# con la funcion summary
summary(mod1)

# Calculamos la tabla ANOVA
anova(mod1)

# Significacion individual de las variables explicativass

# Para x1 no hay evidencia suficiente para rechazar la hip nula al 5% de significacion

# Repetimos el procedimiento anterior para otras percepciones x2...x7

# Las únicas percepciones para las que se rechazan la hipotesis nula al
# 5% de significacion son:
# flexprec y servconj, ya que sus p-valores se quedan por debajo de 0.05
# Para el resto de percepciones no evidencia suficiente de los datos
# para rechazar la hipotesis nula al 5% de signifcicacion, por lo que la 
# percepcion del cliente ante dichas percepciones no parece influir
# significativamente en su fidelidad

# Significacion del termino constante

# Con el 5% no podríamos presencidir del termino constante en el modelo
# ya que su p-valor es 0.044, en cambio para el 1% si se podria


# Homocedasticidad

# Calculamos los residuos
residuos<-rstandard(mod1)
y_gorro<-mod1$fitted.values

# Representamos el grafico de dispersion de los pares (y_gorro,residuos)
plot(y_gorro,residuos)

# Represenntamos para cada percepcion x_j los grafos de dispersion de los pares
# (x_ij,r_i)

plot(hatco$empresa, residuos)
plot(hatco$velocidad, residuos)
plot(hatco$precio, residuos)
plot(hatco$flexprec, residuos)
plot(hatco$imgfabri, residuos)
plot(hatco$servconj, residuos)
plot(hatco$imgfvent, residuos)
plot(hatco$calidadp, residuos)
plot(hatco$fidelidad, residuos)

# Podemos observar que no hay ningun patron excepto en el ultimo grafico

# Borramos memoria

rm(list=ls(all=TRUE))
