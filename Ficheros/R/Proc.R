#Regresion con la que se hace la prediccion
step
#Datos para la predicion
mydata <- cbind(data.frame(t_sT),data.frame(o_eT),data.frame(sprT))
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

#K-fold cross-validation
library(DAAG)
kfcv = cv.lm(mydata, step, m=3)
