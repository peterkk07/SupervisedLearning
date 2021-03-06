#Cambiar ruta
setwd("C:/Users/pedro/Desktop/mineria")


# funcion para instalar paquetes 
install = function(pkg)
{
  # Si ya est� instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}


install("class")
install("dplyr")
install("rpart")
install("rpart.plot")
install("RWeka")






library("rpart")
library("rpart.plot")
library("class")
library("RWeka")
library("dplyr")

data <- read.csv("minable.csv", stringsAsFactors = FALSE)



#Borrar columnas innecesarias y outliers




data[,"jReprobadas"] = NULL
data[,"dHabitacion"] = NULL
data[,"sugerencias"] = NULL
data[,"aEconomica"] = NULL
data[,"cDireccion"] = NULL
data[,"oSolicitudes"] = NULL
data[,"pReside"] = NULL
data[,"cIdentidad"] = NULL
data[,"rating"] = NULL
data[,"fNacimiento"] = NULL






data$grOdontologicos[data$grOdontologicos == "o"] <- 0
data$grOdontologicos <- as.numeric (data$grOdontologicos)
data$mIngreso <- as.factor(data$mIngreso)





data_norm <- as.data.frame(lapply(data[-5], normalize))

#data de prueba y entrenamiento

data_train <- data_norm[1:110,]
data_test <- data_norm[111:190,]




data_train_l <- data[1:110, 5]
data_test_l <- data[111:190, 5]

#modelos k vecinos  (4 modelos)
data_predict0 <- knn(train = data_train, test = data_test, cl = data_train_l, k= 8)
data_predict1 <- knn(train = data_train, test = data_test, cl = data_train_l, k= 9)
data_predict2 <- knn(train = data_train, test = data_test, cl = data_train_l, k= 10)
data_predict3 <- knn(train = data_train, test = data_test, cl = data_train_l, k= 11)


#matrices de confusion

Matriz0 <- table(data_test_l, data_predict0)
matriz_precision <- sum(diag(Matriz0))/sum(Matriz0)

Matriz1 <- table(data_test_l, data_predict1)
matriz_precision1 <- sum(diag(Matriz1))/sum(Matriz1)

Matriz2 <- table(data_test_l, data_predict2)
matriz_precision2 <- sum(diag(Matriz2))/sum(Matriz2)


Matriz3 <- table(data_test_l, data_predict3)
matriz_precision3 <- sum(diag(Matriz2))/sum(Matriz2)



#matrices de precision para distintos valores de k
Matriz_precision 
Matriz_precision1 
Matriz_precision2 
Matriz_precision3 




# �rboles de Decisi�n
arbol_training <- sample_n(data,128)
arbol_test <- sample_n(data,62)

arbol <- rpart(mIngreso ~ ., arbol_training, method = "class", control = rpart.control(minsplit = 10, cp = 0.01))
rpart.plot(arbol)

arbol1 <- rpart(mIngreso ~ ., arbol_training, method = "class", control = rpart.control(minsplit = 11, cp = 0.01))
rpart.plot(arbol)

arbol2 <- rpart(mIngreso ~ ., arbol_training, method = "class", control = rpart.control(minsplit = 13, cp = 0.00001))
rpart.plot(arbol)

arbol3 <- rpart(mIngreso ~ ., arbol_training, method = "class", control = rpart.control(minsplit = 15, cp = 0.000001))
rpart.plot(arbol)




# Matriz

Matrizt <- table(arbol_test$mIngreso, predict(arbol, newdata = arbol_test,type = "class"))
Matrizt_e <- sum(diag(Matrizt))/sum(Matrizt)
 

Matrizt2 <- table(arbol_test$mIngreso, predict(arbol1, newdata = arbol_test,type = "class"))
Matrizt_e2 <- sum(diag(Matrizt2))/sum(Matrizt2)

Matrizt3 <- table(arbol_test$mIngreso, predict(arbol2, newdata = arbol_test,type = "class"))
Matrizt_e3 <- sum(diag(Matrizt3))/sum(Matrizt3)


Matrizt4 <- table(arbol_test$mIngreso, predict(arbol3, newdata = arbol_test,type = "class"))
Matrizt_e3 <- sum(diag(Matrizt3))/sum(Matrizt3)



# precision para distintos arboles
Matrizt_e <- sum(diag(Matrizt))/sum(Matrizt)
Matrizt_e2 <- sum(diag(Matrizt2))/sum(Matrizt2)
Matrizt_e3 <- sum(diag(Matrizt3))/sum(Matrizt3)
Matrizt_e3 <- sum(diag(Matrizt3))/sum(Matrizt3)

































