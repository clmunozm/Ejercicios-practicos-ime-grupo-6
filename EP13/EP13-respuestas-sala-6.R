# EP-13
# Inf. y Modelos Estadísticos
# Arturo Cadenas (20.468.370-0)
# Claudio Muñoz (20.003.395-7)
# Bryan Salas (19.316.410-2)
# Miguel Salinas (20.215.515-4)

# Librerías
if (!require(readxl) ) {
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}
if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}
if (!require(corrplot)){
  install.packages("corrplot", dependencies = TRUE )
  require (corrplot)
}
if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}
if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE )
  require (pROC)
}


# Se cargan de datos.
datos <- read.csv2(file.choose(),header=TRUE)
datos_num <- as.data.frame(apply(datos, 2, as.numeric))
end<-nrow(datos)
# Se calcula el IMC
valores_IMC <- as.data.frame(datos_num$Weight[1:end]/((datos_num$Height[1:end]/100)^2))
colnames(valores_IMC) <- "IMC"
datos <- cbind(datos, valores_IMC) #Se agrega el IMC a los datos

# Se determina el estado nutricional (EN) y se agregan a los datos
datos <- mutate(datos, EN= as.integer(IMC >= 25))

datos_num <- as.data.frame(apply(datos, 2, as.numeric))

# Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.
predictoras <- c("Height",
                 "Ankle.Minimum.Girth",
                 "Forearm.Girth",
                 "Wrists.diameter",
                 "Chest.depth",
                 "Knee.Girth",
                 "Biacromial.diameter",
                 "Biiliac.diameter")



# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN,
# justificando bien esta selección.

correlacion <- round(cor(datos_num, datos_num$EN), 2)
correlacion

# Obtenemos que las variables con mayor correlación con EN son:
# EN = 1; IMC = 0.80; Waist.Girth = 0.67; Weight = 0.66; Hip.Girth = 0.65.
# La variable EN se determina a partir del valor de IMC, por lo que omitiremos esta variable ya que sería lo mismo que ajustar un modelo 
# con la misma variable que buscamos predecir.
# 
# De esta manera seleccionamos la variable Waist.Girth, pues es la siguiente con mayor correlación; siendo esta es de tipo directa
# y además se suele usar para estimar la cantidad de grasa corporal, lo que se relaciona con el peso de una persona.

predictor <- "Waist.Girth"


# Usando el entorno R, construir un modelo de regresión logística con el predictor seleccionado en el paso
# anterior.

datos_num$EN <- factor(datos_num$EN)

# Ajustar modelo usando validaciónn cruzada

modelo <- train(EN ~ Waist.Girth, data = datos_num, method = "glm",
                family = binomial(link = "logit"),
                trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
                )

print(summary(modelo))

# Evaluar el modelo
cat("Evaluación del modelo basada en validación cruzada :\n")
matriz <- confusionMatrix(modelo$pred$pred, modelo$pred$obs)
print(matriz)


# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 3

f <- as.formula(paste("EN", paste(c(predictor, predictoras), collapse = "+"), sep = " ~ "))
print(f)

modelo.completo <- train(f, data = datos_num, method = "glm",
                        family = binomial(link = "logit"),
                        trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
                        )
print(summary(modelo.completo))

# Evaluar el modelo completo
cat("Evaluación del modelo completo basada en validación cruzada :\n")
matriz.modelo.completo <- confusionMatrix(modelo.completo$pred$pred, modelo.completo$pred$obs)
print(matriz.modelo.completo)

# No soporta los modelos ajustados con Train...
modelo.escalonado = step(modelo, scope = list(lower = modelo, upper = modelo.completo), direction = "both", trace = 0)


# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben
# cumplir.




# Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando validación 
# cruzada) y revisar las respectivas curvas ROC.


# Evaluar el modelo
cat("Evaluación del modelo basada en validación cruzada :\n")
matriz <- confusionMatrix(modelo$pred$pred, modelo$pred$obs)
print(matriz)

roc.modelo <- roc(datos_num[["EN"]], as.numeric(modelo$pred$pred))
plot(roc.modelo)


# Evaluar el modelo completo
cat("Evaluación del modelo completo basada en validación cruzada :\n")
matriz.modelo.completo <- confusionMatrix(modelo.completo$pred$pred, modelo.completo$pred$obs)
print(matriz.modelo.completo)

roc.modelo.completo <- roc(datos_num[["EN"]], as.numeric(modelo.completo$pred$pred))
plot(roc.modelo.completo)









