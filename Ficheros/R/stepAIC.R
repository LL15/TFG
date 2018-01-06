##Regresión lineal con varias variables
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
rm(sumOE); rm(nOE)
for(i in 1:769){
  if(o_e[i] == 0){
    o_e[i] = mOE_sin_0
  }
}
rm(i); rm(mOE_sin_0);

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
rm(sumTS); rm(nTS)
for(i in 1:769){
  if(t_s[i] == 0){
    t_s[i] = mTS_sin_0
  }
}
rm(i); rm(mTS_sin_0);

#Las variables textuales se transforman en numericas
comp <- as.numeric(df$Component_s)
i_ty <- as.numeric(df$Issue_Type)
asig <- as.numeric(df$Assignee)
spr <- as.numeric(df$Sprint)
ep_link <- as.numeric(df$Epic_Link)
pr <- as.numeric(df$Priority)
for (i in 1:769){ #Ordenacion de la prioridad
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

#Toda la separación de los datos si se ha hecho las pruebas
#con las regresiones lineales simples, podría no hacerse
set.seed(1234)
ind <- sample(2, 769, replace = TRUE, prob = c(0.7, 0.3));
ind <- as.numeric(ind)
#Se usa el vector aleatorio ya creado para dividir los datos
# (en entrenamiento y test)

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

rm(asig);rm(comp);rm(ep_link); rm(t_s)
rm(i_ty);rm(o_e); rm(pr); rm(spr)

library(MASS)

#Regresiones lineales
#Recta con variables con correlacion positiva
#Se hace la recta de regresion
regPos <-lm (t_sE ~ o_eE + prE + compE + ep_linkE)
summary(regPos)
#Se selecciona un conjunto de variables
stepPos <- stepAIC(regPos, direction="both")
stepPos$anova # display results

#Recta con variables con correlacion negativa
#Se hace la recta de regresion
regNeg <-lm (t_sE ~ i_tyE + asigE + sprE)
summary(regNeg)
#Se selecciona un conjunto de variables
stepNeg <- stepAIC(regNeg, direction="both")
stepNeg$anova # display results

#Regresion lineal con todas las variables
#Se hace la recta de regresion
reg <-lm (t_sE ~ o_eE + prE + compE + ep_linkE + i_tyE + asigE + sprE)
summary(reg)
#Se selecciona un conjunto de variables
step <- stepAIC(reg, direction="both")
step$anova # display results
