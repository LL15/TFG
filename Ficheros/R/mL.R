#Librerias para trabajar con XML
library("XML")
library("methods")
#Lectura de los datos
df <- xmlToDataFrame("C:\\resulQuery.xml")

#Carga de los datos con los que trabajar

#Vector numerico del tiempo original estimado de las tareas
o_e <- as.numeric(as.character(df$Original_Estimate))
#Vector numerico del tiempo gastado en las tareas
t_s <- as.numeric(as.character(df$Time_Spent))
#Los siguientes vectores surgen del cambio de un campo de tipo textual
#a un campo de tipo numerico.
#Vector con la prioridad de las tareas
prior <- as.character(df$Priority)
#Vector numerico que representa el tipo de las tareas
i_t <- as.numeric(df$Issue_Type)
#Vector numerico que representa el link de la epica de las tareas
e_l <- as.numeric(df$Epic_Link)
#Vector numerico representante del sprint de las tareas
spr <- as.numeric(df$Sprint)
#Vector numerico que representa las personas asignadas a cada tarea
asig <- as.numeric(df$Assignee)

#Transformacion de lo vectores

#Calculo de la media del tiempo estimado para las tareas segun tipo
#de tarea
medOE <- tapply(as.numeric(as.character(df$Original_Estimate)), df$Issue_Type, mean)
#Media del tiempo estimado de las tareas
mediaOE = mean(medOE)
#Cuando el tiempo estimado es 0 se cambia por la media
for(i in 1:769){
  if(o_e[i]==0){
    o_e[i] = mediaOE
  } 
}

#Calculo de la media del tiempo gastado en las tareas segun tipo
#de tarea
medTS <- tapply(as.numeric(as.character(df$Time_Spent)), df$Issue_Type, mean)
#Media del tiempo gastado en las tareas
mediaTS = mean(medTS)
#Cuando el tiempo estimado es 0 se cambia por la media
for(i in 1:769){
  if(t_s[i]==0){
    t_s[i] = mediaTS
  } 
}

#Transformacion del vector de prioridades en vector numerico
for(i in 1:769){
  if(prior[i]=="Minor"){
    prior[i] = 1
  } 
  if(prior[i]=="Major"){
    prior[i] = 2
  } 
  if(prior[i]=="Blocker"){
    prior[i] = 3
  } 
  if(prior[i]=="Critical"){
    prior[i] = 4
  } 
}
prior <-as.numeric(prior)

#Union, por columnas, de los datos que se usaran
datos <- cbind(o_e, i_t, spr, prior, e_l, t_s)
n <- nrow(datos)

#Librerias para trabajar con redes neuronales
library(MASS)
library(neuralnet)
library(ggplot2)

#Muestras de los datos con las que se va a trabajar
muestra  <- sample(n, n * .70)
#Datos de entrenamiento
train    <- datos[muestra, ]
#Datos de test
test     <- datos[-muestra, ]

# Normalizacion de los datos
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]

#Formula para entrenar el modelo
nms  <- names(train_nrm)
#Quitar la columna t_s porque es la que se quiere predecir a partir
# de las demas columnas.
form <- as.formula(paste("t_s ~",
                         paste(nms[!nms %in% "t_s"],
                               collapse = " + ")))

#Modelo de red con el que se entrena
rN <- neuralnet(form, data = train_nrm, hidden = c(5,4,3,2))

#Modelo de red con el que se entrena
#rN <- neuralnet(form, data = train_nrm, hidden = c(5,3,2))
#Modelo de red con el que se entrena
#rN <- neuralnet(form, data = train_nrm, hidden = c(5,2))

# Prediccion haciendo uso de los datos de test
prediccion <- compute(rN,within(test_nrm,rm(t_s)))

# Desnormalizacion de los resultados
t_s.pred  <- prediccion$net.result*(max(t_s)-min(t_s))+min(t_s)
t_s.real  <- (test_nrm$t_s)*(max(t_s)-min(t_s))+min(t_s)

#Suma del error cuadratico
error_cuadr <- sum((t_s.real - t_s.pred)^2)/nrow(test_nrm)
#Errores
qplot(x=t_s.real, y=t_s.pred, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Suma de Error Cuadratico=", round(error_cuadr,2)))

#Grafico de la red
plot(rN)

#K-fold para todos los modelos