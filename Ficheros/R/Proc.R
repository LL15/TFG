#Regresion con la que se hace la prediccion
step
#Datos de entrenamieto y test
train <- data.frame(cbind(t_sE,o_eE,sprE))
test <- data.frame(cbind(t_sT,o_eT,sprT))

#K-fold cross-validation
library(DAAG)
kfcv = cv.lm(train, step, m=5)

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