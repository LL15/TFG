#Carga de bibliotecas para lectura de XML
library("XML")
library("methods")
#Recuperacion de los datos del XML
df <- xmlToDataFrame("C:\\resulQuery.xml")

#Aplicación del mejor algoritmo de clusterizacion
km=kmeans(matrizCorr, centers = 10)

#Se muestran los cluster de dos en dos variables
plot(o_e, t_s, col=km$cluster)
points(km$centers, cex=2, col=11, pch=19)
points(matrix(colMeans(matrizCorr), nrow=1, ncol=2),cex=3, col=12, pch=19)

#Se muestran los graficos de los datos con los colores de los cluster
plot(pr, t_s, col=km$cluster)
plot(i_ty, t_s, col=km$cluster)
plot(comp, t_s, col=km$cluster)
plot(asig, t_s, col=km$cluster)
plot(spr, t_s, col=km$cluster)
plot(ep_link, t_s, col=km$cluster)