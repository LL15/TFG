#Regresion con la que se hace la prediccion
step
#Regresión para todos los datos con la que se hara la kf cv
reg_lineal <- lm(t_s ~ o_e + spr)
#Datos de entrenamieto y test
train <- data.frame(cbind(t_sE,o_eE,sprE))
test <- data.frame(cbind(t_sT,o_eT,sprT))

#Conjunto total de datos
total <- data.frame(cbind(t_s,o_e,spr))

#K-fold cross-validation
library(lattice)
library(DAAG)
kfcv10 = cv.lm(total, reg_lineal, m=10)
kfcv5 = cv.lm(total, reg_lineal, m=5)

#Prediccion
x <- predict.lm(step, mydata)
x <- as.numeric(x)

error = 0; error_medio = 0
for(i in 1:235){
  error = t_sT[i]^2-x[i]^2
  error_medio = error_medio + error
}
rm(i)
error = as.numeric(error)
error_medio = as.numeric(error_medio)