#Librerias para trabajar con XML
library("XML")
library("methods")
#Lectura de los datos
df <- xmlToDataFrame("C:\\resulQuery.xml")

#Vector numerico del tiempo original estimado de las tareas
o_e <- as.numeric(as.character(df$Original_Estimate))
#Histogramas del tiempo original estimado
  #Sabiendo el numero de diferentes valores que hay
hist(o_e, main="Histograma del tiempo original estimado", 
     xlab="Tiempo estimado", breaks = 225, col="orange")
  #Sin poner el numero de diferentes valores que hay
hist(o_e, main="Histograma del tiempo original estimado", 
     xlab="Tiempo estimado", col="orange")
hist(o_e, main="Histograma del tiempo original estimado", 
     xlab="Tiempo estimado", col="orange", prob = TRUE)
lines(density(o_e))
hist(o_e, main="Histograma del tiempo original estimado", xlim = c(0, 20000),
     xlab="Tiempo estimado", breaks = 145, col="orange", prob = TRUE)
lines(density(o_e))

#Cuartiles
quantile(o_e)
quantile(o_e, seq(0, 1, 0.005))

#Media de todos los valores
medOE = mean(o_e)
#Media de los valores que no son 0
sumOE = 0; nOE = 0
for(i in 1:769){
  if(o_e[i] != 0){
    sumOE = sumOE + o_e[i]
    nOE = nOE + 1
  }
}
rm(i)
mOE_sin_0 = sumOE/nOE

#Reemplazo de los valores
for(i in 1:769){
  if(o_e[i] == 0){
    o_e[i] = mOE_sin_0
  }
}
rm(i)

#Vector numerico del tiempo original estimado de las tareas
t_s <- as.numeric(as.character(df$Time_Spent))
#Histogramas del tiempo original estimado
  #Sabiendo el numero de diferentes valores que hay
hist(t_s, main="Histograma del tiempo empleado", breaks = 554,
     xlab="Tiempo estimado", col="orange")
  #Sin poner el numero de diferentes valores que hay
hist(t_s, main="Histograma del tiempo empleado", 
     xlab="Tiempo estimado", col="orange", prob = TRUE)
lines(density(t_s))
hist(t_s, main="Histograma del tiempo empleado", breaks = 519,
     xlab="Tiempo estimado", xlim = c(0, 50000),
     col="orange", prob = TRUE)
lines(density(t_s))

#Cuartiles
quantile(t_s)
quantile(t_s, seq(0, 1, 0.005))

#Media de todos los valores
medTS = mean(t_s)
#Media de los valores que no son 0
sumTS = 0; nTS = 0
indices = NULL;
for(i in 1:769){
  if(t_s[i] != 0){
    sumTS = sumTS + t_s[i]
    nTS = nTS + 1
    indices = rbind(indices, i)
  }
}
rm(i)
mTS_sin_0 = sumTS/nTS
#Obtencion del vector con los indices de los datos nulos
indices = as.vector(indices[,1])

#Reemplazo de los valores nulos
for(i in 1:769){
  if(t_s[i] == 0){
    t_s[i] = mTS_sin_0
  }
}
rm(i); rm(mTS_sin_0);
rm(sumTS); rm(nTS)
