  # Programación en Paralelo
  
  library(doParallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

# Modelos, Tiempo de Procesamiento y Matriz de Confusión
library(caret)
iteracion <- createFolds(datos_d$causa67A, k = 10) #Bloques CV
tr_control <- trainControl(method = "cv", number = 10, index = iteracion, allowParallel = T) #Control CV

set.seed(5032013) #SVM modelo
tsvm_2 <- system.time(svm_model_2 <- train(data = datos_d,
                                           causa67A ~.,
                                           method = "svmLinear",
                                           trainControl = tr_control,
                                           tuneLenght = 200))
tsvm_2
confusionMatrix(svm_model_2)

set.seed(5032013) #Nnet modelo
tnnet_2 <- system.time(nnet_model_2 <- train(data = datos_d,
                                             causa67A ~.,
                                             method = "nnet",
                                             trainControl = tr_control,
                                             tuneLenght = 200))

tnnet_2
confusionMatrix(nnet_model_2)

set.seed(5032013) # XGB Modelo
txgb_2 <- system.time(xgb_model_2 <- train(data = datos_d,
                                           causa67A ~.,
                                           method = "xgbTree",
                                           trainControl = tr_control,
                                           tuneLenght = 200))

txgb_2

confusionMatrix(xgb_model_2)

# Mejores modelos de entre 200 (Gráficos)
svm_model_2$bestTune
plot.train(svm_model_2)

nnet_model_2$bestTune
plot.train(nnet_model_2)

xgb_model_2$bestTune
plot.train(xgb_model_2)

#Deterner programación paralela
registerDoSEQ()
stopCluster(cl)

# Importancia de Variables

varImp(svm_model_2, scale = F)
varImp(nnet_model_2,scale = F)
varImp(xgb_model_2, scale = F)
