#Ejemplo de el siguiente sitio: https://yuasaavedraco.github.io/Docs/Redes_Neuronales_con_R.html#red_neuronal_con_dos_capas

file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE) 
head(datos) # Muestra las 6 primeras filas

library(ggplot2)
ggplot(datos, aes(x=Edad, y=Resistencia)) + geom_point()

maxs <- apply(datos, 2, max) 
mins <- apply(datos, 2, min)
scaled <- as.data.frame(scale(datos, center=mins, scale=maxs-mins))

head(cbind(datos, scaled))

ggplot(scaled, aes(x=Edad, y=Resistencia)) + geom_point()

library(neuralnet)
mod1 <- neuralnet(Resistencia ~ Edad, data=scaled, 
                  hidden=c(1), threshold=0.01)

plot(mod1, rep="best")

names(mod1)

mod1$act.fct          # Activation function

unlist(mod1$weights)  # To obtain weights in vector form

test <- data.frame(Edad = scaled$Edad)
myprediction <- compute(x=mod1, covariate=test)

myprediction$net.result[1:5]

yhat_red <- myprediction$net.result * (max(datos$Resistencia)-min(datos$Resistencia))+min(datos$Resistencia)
datos$yhat_red <- yhat_red
yhat_red[1:5] # Para ver los primeros 5 valores estimados

ggplot(datos, aes(x=Resistencia, y=yhat_red)) + geom_point() +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dashed", size=1.5)

mod2 <- lm(Resistencia ~ Edad, data=datos)
coef(mod2)

yhat_mls <- fitted(mod2)
yhat_mls[1:5] # Para ver los primeros 5 valores estimados

ecm_red <- mean((datos$Resistencia - yhat_red)^2)
ecm_red

ecm_rls <- mean((datos$Resistencia - yhat_mls)^2)
ecm_rls

mod3 <- neuralnet(Resistencia ~ Edad, data=scaled, 
                  hidden=c(2, 3), threshold=0.01)

plot(mod3, rep="best")