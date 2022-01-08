# EP-12
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

# Se cargan de datos.
datos <- read.csv2(file.choose(),header=TRUE, sep = "")

# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas mediciones
# están disponibles en el archivo Body-EP12.csv que acompaña a este enunciado. El estudio incluyó nueve
# mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce mediciones de grosor
#(circunferencias) que incluyen el tejido.

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso.
set.seed(8370)

# Como la semilla se trata de un numero par se filtra la tabla dejando solo mujeres
tabla <- filter(datos, Gender == 0)

#Se hace una copia de la tacle y se elimina la variable peso
aux <- tabla
aux$Weight <-NULL
#Se obtienen los nombres de las varaibles
variables<-colnames(aux)
#Se seleccionan al azar 8 varaibles predictoras
predictoras<-sample(variables, size = 8)
print(predictoras)





# Se pasan los datos a tipo numérico
datos_num <- as.data.frame(apply(tabla, 2, as.numeric))

# Tabla de Correlación
tabla_correlacion <- cor(x)

# Predictor Elegido: Waist.Girth -> Grosor a la altura de la cintura en cm
# Pues tiene la correlacion más alta, esta es de tipo directa.
cor(datos_num$Waist.Girth, datos_num$Weight)

# Muestra de 50 mujeres
datos_num <- datos_num[sample(nrow(datos_num), 50),]

# Ajustar modelo con R.
modelo <- lm(Weight ~ Waist.Girth, data = datos_num)
print(summary(modelo))

# Graficar el modelo.
p <- ggscatter(datos_num, x = "Waist.Girth", y = "Weight", color = "blue", fill = "blue",
               xlab = "Grosor a la altura de la cintura [cm]", ylab = "Peso [Kg]")

p <- p + geom_smooth(method = lm, se = FALSE , colour = "red")
print(p)

# Crear gráficos para evaluar el modelo .
plot(modelo)
