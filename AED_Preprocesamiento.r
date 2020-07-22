  # Carga de datos y Selección de variables
  datos <- read.csv(file = "EDG 2018.csv", sep = ";", encoding = "UTF-8")
  str(datos)
  datos <- datos[,c("cod_pais",	"sexo",	"edad",	"prov_res",	"area_res",	"est_civil",	"niv_inst",	"sabe_leer",	"etnia",	"lugar_ocur",	"prov_fall",	"causa67A")]
  
  for (i in 1:length(datos)) 
  {
    ifelse(class(datos[,i])=="character", yes = datos[,i]<- as.factor(datos[,i]),no = datos[,i]<-datos[,i])
    for (j in 1:length(levels(datos[,i]))) 
    {
      ifelse(levels(datos[,i])[j] == " "| levels(datos[,i])[j] == "Sin información" , yes = levels(datos[,i])[j] <- NA, no = levels(datos[,i])[j] <- levels(datos[,i])[j])
    }
  }
  
  
  #Criterios de inclusion y exclusion de variables y datos
  
  levels(datos$causa67A)
  sort(table(datos$causa67A),T)[1:2]
  
  datos<- datos[datos$causa67A == levels(datos$causa67A)[7]|
                  datos$causa67A == levels(datos$causa67A)[2]
                
                datos$causa67A <- as.character(datos$causa67A)
                datos$causa67A <- as.factor(datos$causa67A)
                levels(datos$causa67A)
                
                #quedan 30324 datos despues de aplicados los criterios
                ind <- which(datos$edad == 999 )
                datos$edad[c(ind)] <- rep(NA,3)
                
                # Descriptivos EDAD - inicial
                
                summary(datos$edad)
                sd(datos$edad,na.rm = T)/mean(datos$edad, na.rm =T)
                round(prop.table(table(datos$causa67A, useNA = "always")),digits = 4)*100
                
                
                # Detectar y  Remover NA's 
                
                sum(apply(datos, MARGIN = 1, function(x) any(is.na(x))))# Cuenta el Número de filas con Na's = 4326
                posicion <- 1:nrow(datos) # Crea un vector de posiciones con el número de filas del dataframe
                nas <- posicion[apply(datos, MARGIN = 1, function(x) any(is.na(x)))];nas # Filas con NA's
                datos<- datos[-nas,] # Eliminación de filas con NA's.. Hay 25998 datos sin nas
                sum(apply(datos, MARGIN = 1, function(x) any(is.na(x)))) # Compraobacion que no hay NAS = 0
                
                
                #Recategorización de Variables
                
                #Recategorización - País
                
                ParetoDiagram(datos$cod_pais,limite = 95)+coord_flip()
                levels(datos$cod_pais)[-15] <- "Otros"
                levels(datos$cod_pais)
                
                #Recategorización - Región de Residencia
                
                levels(datos$prov_res)[c(1,2,3,4,5,6,12,13,20,24)] <- "Sierra"
                levels(datos$prov_res)[c(2,3,6,7,8,13,14)] <- "Costa"
                levels(datos$prov_res)[c(5,6,7,8,9,10)] <- "Amazonía"
                levels(datos$prov_res)[4] <- "Insular"
                names(datos)[4] <- "reg_res"
                ParetoDiagram(datos$reg_res, 98)
                levels(datos$reg_res)[-c(1,2)] <- "Otras"
                levels(datos$reg_res)
                
                
                #Recategorización - Estado Civil
                
                ParetoDiagram(datos$est_civil,95)
                levels(datos$est_civil)[c(2,4)] <- "Otros"
                levels(datos$est_civil)
                
                #Recategorización - Nivel de Instrucción
                
                levels(datos$niv_inst)[c(1,2,6)] <- "Educación Básica/Primaria/Alfabetizado"
                levels(datos$niv_inst)[c(2,5)] <- "Educación Media/Bachillerato/Secundaria"
                levels(datos$niv_inst)[c(4,5,6)] <- "Superior/Postgrado"
                levels(datos$niv_inst)
                ParetoDiagram(datos$niv_inst,95)
                
                #Recategorización - Etnia
                
                levels(datos$etnia)[1] <- "Afrodescendiente"
                levels(datos$etnia)
                ParetoDiagram(datos$etnia, 95)
                levels(datos$etnia)[-c(4,5)] <- "Otras"
                levels(datos$etnia)
                
                
                #Recategorización - Provincia de Fallecimiento
                
                levels(datos$prov_fall)[c(1,2,3,4,5,6,12,13,20,24)] <- "Sierra"
                levels(datos$prov_fall)[c(2,3,6,7,8,13,14)] <- "Costa"
                levels(datos$prov_fall)[c(5,6,7,8,9,10)] <- "Amazonía"
                levels(datos$prov_fall)[4] <- "Insular"
                
                names(datos)[11] <- "reg_fall"
                
                ParetoDiagram(datos$reg_fall, 97.45)
                levels(datos$reg_fall)[-c(1,2)] <- "Otros"
                levels(datos$reg_fall)
                
                
                #Recategorización - Lugar de Ocurrencia
                
                levels(datos$lugar_ocur)[c(2,3,4)] <- c("Junta Beneficencia"," IESS","Estbl. Min. Salud")
                ParetoDiagram(datos$lugar_ocur,95)
                levels(datos$lugar_ocur)[c(2,3,6,7)] <- "Otros"
                levels(datos$lugar_ocur)
                
                
                # Frecuencias post-recategorizaicón
                
                table(datos$est_civil)/ sum(table(datos$est_civil))*100
                table(datos$niv_inst)/ sum(table(datos$niv_inst))*100
                table(datos$sabe_leer)/ sum(table(datos$sabe_leer))*100
                table(datos$etnia)/ sum(table(datos$etnia))*100
                
                
                #Escalado y Descriptivos EDAD
                
                datos$edad <- (datos$edad - min(datos$edad))/(max(datos$edad) - min(datos$edad))
                mean(datos$edad)
                median(datos$edad)
                sd(datos$edad)
                sd(datos$edad)/mean(datos$edad)
                
                
                #Oversampling
                library(tidyverse)
                V030 <- datos %>%
                  filter(causa67A == "030 Enfermedades del sistema circulatorio")
                
                V016 <- datos %>%
                  filter(causa67A == "016 Tumores [neoplasias]")
                
                
                set.seed(5032013)
                indice1 <- sample(1:nrow(V016), size = 15467, replace = T)
                V016 <-  V016[indice1,]
                
                datos_d <- rbind(V030,V016)
                
                
                
