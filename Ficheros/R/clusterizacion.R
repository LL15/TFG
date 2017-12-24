#Carga de bibliotecas para lectura de XML
library("XML")
library("methods")
#Recuperacion de los datos del XML
df <- xmlToDataFrame("C:\\resulQuery.xml")
#Separacion de algunos datos numericos
o_e <- df[ , "Original_Estimate"]
t_s <- df[ , "Time_Spent"]
#Matriz con la que se trabajara
datos <- cbind(o_e,t_s)

#Aplicación del mejor algoritmo de clusterizacion
km=kmeans(datos, centers = 10)
plot(datos, col=km$cluster)
points(km$centers, cex=2, col=11, pch=19)
points(matrix(colMeans(datos), nrow=1, ncol=2),cex=3, col=12, pch=19)
