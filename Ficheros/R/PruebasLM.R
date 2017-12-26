#Las variables textuales se transforman en numericas
comp <- as.numeric(df$Component_s)
i_ty <- as.numeric(df$Issue_Type)
asig <- as.numeric(df$Assignee)
spr <- as.numeric(df$Sprint)
ep_link <- as.numeric(df$Epic_Link)
pr <- as.numeric(df$Priority)
for (i in 1:769){
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
cor(t_s, o_e, pr)

#Recta de la regresion lineal
  # Valores de coeficientes estimados son la ordenada en el origen y la
  #pendiente de la recta
  # Multiple R-Squared coeficiente de determinacion

#Sacar conclusiones a partir de los valores

#Comando predict para predecir valores a partir de las variables con
#las que se calcule la recta