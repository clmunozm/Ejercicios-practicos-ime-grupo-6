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
