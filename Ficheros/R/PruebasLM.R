#Modificacion de la pririodad
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
#Matriz de correlacion entre las variables
  # Valores de coeficientes estimados son la ordenada en el origen y la
  #pendiente de la recta
  # Multiple R-Squared coeficiente de determinacion

#Sacar conclusiones a partir de los valores

#Comando predict para predecir valores a partir de las variables con
#las que se calcule la recta