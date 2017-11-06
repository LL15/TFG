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


#Determinamos numero optimo de cluster
library("NbClust")
numK = NbClust(datos, method = "kmeans")
numC = NbClust(datos, method = "centroid")


#Se cargan las bibliotecas relacionadas con
#la clusterización
library("cluster")
library("fpc")
#Medicion del tiempo de los diferentes algoritmos
#1 Kmean: 2  cluster
t <- proc.time() # Inicia el cronómetro
km1= kmeans(datos, centers = 2)
proc.time()-t    # Detiene el cronómetro
#2 Kmean: 4 cluster
t <- proc.time() # Inicia el cronómetro
km2= kmeans(datos, centers = 4)
proc.time()-t    # Detiene el cronómetro
#3 kmeanruns
t <- proc.time() # Inicia el cronómetro
km3= kmeansruns(datos)
proc.time()-t    # Detiene el cronómetro
#4 kmean: 10 cluster
t <- proc.time() # Inicia el cronómetro
km4= kmeans(datos, centers = 10)
proc.time()-t    # Detiene el cronómetro

#1 kmedoids: 2  cluster
t <- proc.time() # Inicia el cronómetro
p1= pam(datos, 2)
proc.time()-t    # Detiene el cronómetro
#2 kmedoids: 4 cluster
t <- proc.time() # Inicia el cronómetro
p2= pam(datos, 4)
proc.time()-t    # Detiene el cronómetro
#3 pamk
t <- proc.time() # Inicia el cronómetro
p3= pamk(datos)
proc.time()-t    # Detiene el cronómetro
#4 kmedoids: 10 cluster
t <- proc.time() # Inicia el cronómetro
p4= pam(datos,10)
proc.time()-t    # Detiene el cronómetro
#5 pamk
t <- proc.time() # Inicia el cronómetro
p5= pamk(datos, usepam = FALSE)
proc.time()-t    # Detiene el cronómetro