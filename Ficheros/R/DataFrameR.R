#Carga de bibliotecas para lectura de XML
library("XML")
library("methods")
#Recuperacion de los datos del XML
result <- xmlParse(file = "C:\\resulQuery.xml")
df <- xmlToDataFrame("C:\\resulQuery.xml")
#Separacion de algunos datos numericos
o_e <- df[ , "Original_Estimate"]
t_s <- df[ , "Time_Spent"]
#Matriz con la que se trabajara
datos <- cbind(o_e,t_s)
#Se cargan las bibliotecas relacionadas con
#la clusterización
library("cluster")
library("fpc")
