# EP-09
# Inf. y Modelos Estadísticos

# Librerías
library ( tidyverse )
library ( ggpubr )
library ( nlme )
library ( emmeans )
library ( ez )

# Pregunta 1
# El siguiente código R carga los datos que aparecen en una tabla que compara las 
# mejores soluciones encontradas por cuatro algoritmos para instancias del problema 
# del vendedor viajero con solución óptima conocida, tomados desde una memoria de 
# título del DIINF. Con estos datos responda la pregunta de investigación: 
# ¿Hay algoritmos mejores que otros?

texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 26 15.6 16.3 17.2 19
'brock400_4' 30 13.8 16.3 17.4 19
'C2000.9' 77 54.2 56.6 59.4 63
'c-fat500-10' 123 122 122 122 123
'hamming10-2' 509 340.2 416.1 419.4 509
'johnson32-2-4' 13 13 13 13 13
'keller6' 56 31.5 40 42.5 45.2
'MANN_a81' 1097 1079.2 1079.2 1079.2 1092.9
'p-hat1500-1' 9 3.9 5.1 5.9 7
'p-hat1500-3' 91 72.8 74.7 81.6 83
'san1000' 12 4.6 4.6 4.7 7
'san400_0.7_1' 37 16.6 17.5 17.5 18
'san400_0.9_1' 97 41.1 51.4 53.4 89
'frb100-40' 97 63.4 73.7 77.5 79
'frb59-26-1' 56 36.2 42.9 45.3 45
'1et.2048' 313 229.4 265.4 277.9 289.4
'1zc.4096' 376 270.8 290.2 304.4 325.5
'2dc.2048' 21 12.6 15.7 16.9 18
")

datos <- read.table(textConnection(texto), header = TRUE)

# Llevar dataframe a formato largo .
datos <- datos %>% pivot_longer(c("Optimo", "R", "R2", "R3", "G"),
                                names_to = "algoritmo", 
                                values_to = "tiempo")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])


# Comprobación de normalidad .
g <- ggqqplot(datos , x = "tiempo", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\nProcedimiento ANOVA usando ezANOVA\n\n")

prueba2 <- ezANOVA(data = datos , dv = tiempo , within = algoritmo ,
                   wid = Instancia , return_aov = TRUE )

print ( summary ( prueba2$aov))

cat("Resultado de la prueba de esfericidad de Mauchly :\n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])

cat("\n\nY factores de corrección para cuando no se cumple la\n")
cat("condición de esfericidad:\n\n")
print(prueba2$'Sphericity Corrections')

# Gráfico del tamaño del efecto .
g2 <- ezPlot(data = datos , dv = tiempo , wid = Instancia , within = algoritmo ,
             y_lab = "Tiempo promedio de ejecución [ms]", x = algoritmo )

print(g2)


# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(tiempo ~ algoritmo , data = datos , random = ~1|Instancia )
medias <- emmeans(mixto , "algoritmo")
tukey <- pairs ( medias , adjust = "tukey")

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)


# Pregunta 2
# El siguiente es (un resumen de) la descripción de un famoso experimento:
#  Naming the ink color of color words can be difficult. For example, if asked to 
# name the color of the word "blue" is difficult because the answer (red) conflicts 
# with the word "blue." This interference is called "Stroop Interference" after the 
# researcher who first discovered the phenomenon. This case study is a classroom 
# demonstration. Students in an introductory statistics class were each given three 
# tasks. In the "words" task, students read the names of 60 color words written in 
# black ink; in the "color" task, students named the colors of 60 rectangles; 
# in the "interference" task, students named the ink color of 60 conflicting color
# words. The times to read the stimuli were recorded.
# El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos 
# datos, responda la siguiente pregunta de investigación: ¿hay diferencias en los 
# tiempos entre tareas?

texto <- ("
words colors interfer
9 14 42
13 22 47
21 19 44
13 24 29
23 17 37
17 21 31
15 19 38
26 24 33
18 19 44
21 20 38
16 15 32
21 17 35
18 19 31
17 23 34
16 16 36
19 15 31
")
datos2 <- read.table(textConnection(texto), header = TRUE)