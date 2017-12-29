#Librerias para trabajar con XML
library("XML")
library("methods")
#Lectura de los datos
df <- xmlToDataFrame("C:\\resulQuery.xml")

#Variables numericas
o_e <- as.numeric(as.character(df$Original_Estimate)) #Tiempo estimado
sumOE = 0; nOE = 0
for(i in 1:769){
  if(o_e[i] != 0){
    sumOE = sumOE + o_e[i]
    nOE = nOE + 1
  }
}
rm(i)
mOE_sin_0 = sumOE/nOE
for(i in 1:769){
  if(o_e[i] == 0){
    o_e[i] = mOE_sin_0
  }
}
rm(i); rm(mOE_sin_0);
rm(sumOE); rm(nOE)

t_s <- as.numeric(as.character(df$Time_Spent)) #Tiempo empleado
sumTS = 0; nTS = 0
for(i in 1:769){
  if(t_s[i] != 0){
    sumTS = sumTS + t_s[i]
    nTS = nTS + 1
  }
}
rm(i)
mTS_sin_0 = sumTS/nTS
for(i in 1:769){
  if(t_s[i] == 0){
    t_s[i] = mTS_sin_0
  }
}
rm(i); rm(mTS_sin_0);
rm(sumTS); rm(nTS)

#Las variables textuales se transforman en numericas
comp <- as.numeric(df$Component_s)
i_ty <- as.numeric(df$Issue_Type)
asig <- as.numeric(df$Assignee)
spr <- as.numeric(df$Sprint)
ep_link <- as.numeric(df$Epic_Link)
pr <- as.numeric(df$Priority)
for (i in 1:769){ #Ordenacion de la 
  if(pr[i] == 4){#Minor
    pr[i] = 1
  }
  else if(pr[i] == 3){#Major
    pr[i] = 2
  }
  else if(pr[i] == 1){#Blocker
    pr[i] = 3
  }
  else{#Critical
    pr[i] = 4
  }
}
rm(i)

#Diagrama de dispersion entre las variables sin la linea de
#regresion.
plot(o_e, t_s, main = "Relación entre tiempo estimado y tiempo
     empleado", xlab="Tiempo estimado", ylab = "Tiempo empleado", pch = 4)
plot(pr, t_s, main = "Relación entre prioridad y tiempo empleado",
           xlab="Prioridad", ylab = "Tiempo empleado", pch = 4)
plot(df$Component_s, t_s, main = "Relación entre componente y tiempo
     empleado", xlab="Componente", ylab = "Tiempo empleado", pch = 4)
plot(df$Issue_Type, t_s, main = "Relación entre tipo de tarea y tiempo
     empleado", xlab="Tipo de tarea", ylab = "Tiempo empleado", pch = 4)
plot(df$Assignee, t_s, main = "Relación entre persona encargada y tiempo
     empleado", xlab="Persona encargada", ylab = "Tiempo empleado", pch = 4)
plot(df$Sprint, t_s, main = "Relación entre sprint y tiempo empleado",
     xlab="Sprint", ylab = "Tiempo empleado", pch = 4)
plot(df$Epic_Link, t_s, main = "Relación entre el epic link y tiempo
     empleado", xlab="Epic Link", ylab = "Tiempo empleado", pch = 4)

#Se hace una matriz con todos los datos
matrizCorr = cbind(t_s, o_e, pr, i_ty, comp, asig, spr, ep_link)

#Matriz de correlacion entre las variables
#Graficas
pairs(t_s ~ o_e + pr + i_ty)
pairs(t_s ~ comp + asig)
pairs(t_s ~ spr + ep_link)
#Textual
cor(matrizCorr)

#Semilla para crear un vector aleatorio y dividir los datos en entrenamiento y test
set.seed(1234)
#Vector que dividira los datoses: 70% entrenamiento, 30% test
ind <- sample(2, nrow(matrizCorr), replace = TRUE, prob = c(0.7, 0.3))
ind = as.numeric(ind)

cbind(o_e, ind); cbind(comp, ind); cbind(pr, ind)
cbind(t_s, ind); cbind(ep_link, ind); cbind(spr, ind)
cbind(asig, ind); cbind(i_ty, ind)
#Se dividen los datos entre los datos de entrenamiento y los de test
asigE <- asig[ind == 1]
asigT <- asig[ind == 2]

compE <- comp[ind == 1]
compT <- comp[ind == 2]

ep_linkE <- ep_link[ind == 1]
ep_linkT <- ep_link[ind == 2]

i_tyE <- i_ty[ind == 1]
i_tyT <- i_ty[ind == 2]

o_eE <- o_e[ind == 1]
o_eT <- o_e[ind == 2]

prE <- pr[ind == 1]
prT <- pr[ind == 2]

sprE <- spr[ind == 1]
sprT <- spr[ind == 2]

t_sE <- t_s[ind == 1]
t_sT <- t_s[ind == 2]

#Rectas de regresion lineal
regOE <-lm (t_sE ~ o_eE)
plot(o_eE, t_sE, main = "Recta de regresión entre tiempo
     estimado y tiempo empleado", xlab="Tiempo estimado",
     ylab = "Tiempo empleado", pch = 4)
abline(regOE)
summary(regOE)

regPR <-lm (t_sE ~ prE)
plot(prE, t_sE, main = "Recta de regresión entre
     prioridad y tiempo empleado",
     xlab="Prioridad", ylab = "Tiempo empleado", pch = 4)
abline(regPR)
summary(regPR)

regC <-lm (t_sE ~ compE)
plot(compE, t_sE, main = "Recta de regresión entre componente
     y tiempo empleado", xlab="Componente", ylab = "Tiempo empleado", pch = 4)
abline(regC)
summary(regC)

regTY <-lm (t_sE ~ i_tyE)
plot(i_tyE, t_sE, main = "Recta de regresión entre tipo de tarea
     y tiempo empleado", xlab="Tipo de tarea", ylab = "Tiempo empleado", pch = 4)
abline(regTY)
summary(regTY)

regAS <-lm (t_sE ~ asigE)
plot(asigE, t_sE, main = "Recta de regresión entre persona
     encargada y tiempo empleado", xlab="Persona encargada", ylab = "Tiempo empleado", pch = 4)
abline(regAS)
summary(regAS)

regSPR <-lm (t_sE ~ sprE)
plot(sprE, t_sE, main = "Recta de regresión entre sprint y tiempo empleado",
     xlab="Sprint", ylab = "Tiempo empleado", pch = 4)
abline(regSPR)
summary(regSPR)

regEL <-lm (t_sE ~ ep_linkE)
plot(ep_linkE, t_sE, main = "Recta de regresión entre el epic
     link y tiempo empleado", xlab="Epic Link", ylab = "Tiempo empleado", pch = 4)
abline(regEL)
summary(regEL)

#Predicciones
x = predict.lm(regOE, data.frame(o_eT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(o_eE)
var(o_eT)

x = predict.lm(regPR, data.frame(prT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(prE)
var(prT)

x = predict.lm(regC, data.frame(compT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(compE)
var(compT)

x = predict.lm(regTY, data.frame(i_tyT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(i_tyE)
var(i_tyT)

x = predict.lm(regAS, data.frame(asigT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(asigE)
var(asigT)

x = predict.lm(regSPR, data.frame(sprT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(sprE)
var(sprT)

x = predict.lm(regEL, data.frame(ep_linkT))
x = as.numeric(x)
error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
error_medio/235
var(ep_linkE)
var(ep_linkT)