#Librerias para trabajar con XML
library("XML")
library("methods")
#Lectura de los datos
df <- xmlToDataFrame("C:\\resulQuery.xml")

#Calculo de la media del tiempo estimado para las tareas segun tipo
#de tarea
meds <- tapply(as.numeric(as.character(df$Original_Estimate)), df$Issue_Type, mean)
#Media del tiempo estimado de las tareas
media = mean(meds)

#Vector numerico del tiempo original estimado de las tareas
o_e <- as.numeric(as.character(df$Original_Estimate))
#Cuando el tiempo estimado es 0 se cambia por la media
for(i in 1:769){
 if(o_e[i]==0){
  o_e[i] = media
 } 
}
t_s <- as.numeric(as.character(df$Time_Spent))

#Correlación entre la variables
cor(o_e, t_s)

#Modelo de regresion lineal
mod <- lm(t_s~o_e)
#Grafico de dispersion de las variables
plot(o_e, t_s, main="ScatterPlot")
abline(mod, col=3, lwd=3)

#Analisi de la tabla de varianza
anova(mod)