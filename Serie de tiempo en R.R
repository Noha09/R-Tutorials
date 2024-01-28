## Instalación desde GitHub
library(devtools)
#install_github("hadley/dplyr")

#Invocar librerias
library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Aislamos la fecha de hoy
hoy <- today()

# Nos traemos todas las acciones que podamos
acciones <- getSymbols("MXN=X", from = "2003-01-01", to = hoy, src = "yahoo", auto.assign = F)[,6]
acciones <- na.omit(acciones)

# Imprimimos la grafica real
plot(acciones)

# Formateamos la tabla de acciones
acciones <- as.data.frame(acciones)
acciones$Fecha <- rownames(acciones)
rownames(acciones) <- NULL
names(acciones) = c("Precio", "Fecha")
acciones$Fecha = as.Date(acciones$Fecha)

# Establecemos un rango de fechas optimo
rango_fecha = (hoy + 1) : (hoy + 30)
Precio = as.numeric(NA)
rango_fecha = as.data.frame(cbind(Precio,rango_fecha))
rango_fecha$Fecha = as.Date(rango_fecha$rango_fecha)
rango_fecha$rango_fecha = NULL

acciones <- rbind(acciones, rango_fecha)

# Formateamos el campo de fecha
acciones$Fecha_dup = acciones$Fecha
acciones <- acciones %>% separate(Fecha, c("Año","Mes","Dia"))
acciones$Año = as.numeric(acciones$Año)
acciones$Mes = as.numeric(acciones$Mes)
acciones$Dia = as.numeric(acciones$Dia)

# Creamos una tabla con los dias a predecir
acciones.sc <- as.data.frame(cbind(acciones$Precio, acciones$Fecha_dup, scale(acciones[ ,c(2:4)])))
names(acciones.sc)[1] = "Precio"
names(acciones.sc)[2] = "Fecha"
acciones.sc$Fecha = as.Date(acciones.sc$Fecha)

train = createDataPartition(na.omit(subset(acciones, acciones$Fecha_dup < today()))$Precio, 
                                 p = 0.7, list = F)
test = rbind(acciones[-train,], subset(acciones,acciones$Fecha_dup >= today()) )
test.cs = as.data.frame(cbind(test$Precio, test$Fecha_dup, scale(test[, c(2,3,4)])))
names(test.cs)[1] = "Precio"
names(test.cs)[2] = "Fecha"
test.cs$Fecha = as.Date(test.cs$Fecha)

# Este modelo no es tan preciso
#install.packages("neuralnet")
#install.packages("NeuralNetTools")

library(neuralnet)
library(NeuralNetTools)

# Ejecutamos la red neuronal
mod = neuralnet(formula = Precio ~ Año + Mes + Dia, data = acciones.sc[train,], hidden = 2,
                threshold = 0.01, stepmax = 1e+08, rep = 1, linear.output = TRUE)

plotnet(mod)

pred <- compute(mod, test.cs)

datos = cbind(pred$net.result, test.cs)

# Calculamos el error absoluto y porcentual
error_abs = RMSE(datos$Precio, datos$`pred$net.result`, na.rm = TRUE)
error_por = error_abs / datos[datos$Fecha == max(na.omit(datos)$Fecha),]$Precio

# Comparamos la prediccion con la grafica original
ggplot() + geom_line(data = datos, aes(x = Fecha, y = Precio), color = "blue") + 
  geom_line(data = datos, aes(x = Fecha, y = `pred$net.result`), color = "red")

# Este modelo es mas preciso
#install.packages("randomForest")

library(randomForest)

# Ejecutamos la funcion de random forest
mod_rf = randomForest(Precio ~ Año + Mes + Dia, data = acciones[train,], 
                      type = "regression", ntree = 100)
pred_rf = predict(mod_rf, test)
datos_rf =cbind(pred_rf,test)

# Calculamos el error absoluto y porcentual
error_abs_rf = RMSE(datos_rf$Precio, datos_rf$pred_rf, na.rm = TRUE)
error_por_rf = error_abs_rf / datos_rf[datos_rf$Fecha_dup == max(na.omit(datos_rf)$Fecha_dup),]$Precio

# Comparamos la prediccion con la grafica original
ggplot() + geom_line(data = datos_rf, aes(x = Fecha_dup, y = Precio), color = "blue") +
  geom_line(data = datos_rf, aes(x = Fecha_dup, y = pred_rf), color = "red")
