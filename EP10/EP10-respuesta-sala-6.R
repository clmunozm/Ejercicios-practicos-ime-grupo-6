# EP-10
# Inf. y Modelos Estad√≠sticos
# Arturo Cadenas (20.468.370-0)
# Claudio Mu√±oz (20.003.395-7)
# Bryan Salas (19.316.410-2)
# Miguel Salinas (20.215.515-4)

# Librer√≠as

# Pregunta 1
# Dos art√≠culos reportan el porcentaje de acierto alcanzado por dos algoritmos 
# de clasificaci√≥n, espec√≠ficamente el Bayes ingenuo (C4) y el Bayes ingenuo 
# oculto (C2), en diferentes conjuntos de prueba disponibles en el UCI Machine 
# Learning Repository. ¬øEs uno de los algoritmo mejor que el otro?

texto <-("
 Dataset C2 Dataset C4
 'ecoli' 79.48 'page-blocks' 92.95
 'primary-tumor' 47.52 'squash-unstored' 61.11
 'pasture-production' 85.27 'mushroom' 95.27
 'contact-lenses' 67.77 'segment' 90.74
 'nursery' 93.72 'cmc' 50.49
 'white-clover' 78.73 'soybean' 91.52
 'monks1' 99.44 'credit' 85.67
 'anneal' 97.44 'monks' 61.68
 'solar-flare-C' 87.68 'postoperatie' 66.11
 'kr-s-kp' 91.90 'grub-damage' 47.23
 'tae' 43.82 'waveform' 79.30
 'squash-stored' 57.44 -- --
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

# Hip√≥tesis a contrastar:
# H0: No hay diferencias en el porcentaje de acierto alcanzado por los dos 
#   algoritmos de clasificaci√≥n
# HA: S√≠ hay diferencias en el porcentaje de acierto alcanzado por los dos 
#   algoritmos de clasificaci√≥n

# Establecer nivel de significaci√≥n.
alfa <- 0.05

# Se utiliza la prueba de Suma de rangos de Wilcoxon puesto que se trabaja con dos
# muestras independientes.
# Se verifican el cumplimiento de las condiciones:
# - Las observaciones de ambas muestras son independientes puesto que son extra√≠dos
# de art√≠culos.
# - La escala empleada es ordinal, puesto que se trata de porcentaje de acierto
# alcanzado por los dos algoritmos.

# Se realiza la prueba de Mann-Whitney .
prueba <- wilcox.test (datos$C2, datos$C4, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)

# Conclusi√≥n de la prueba:
# Con respecto al resultado obtenido y con un valor de p = 0.6947, este siendo
# mayor al nivel de significaci√≥n, por lo tanto, se falla al rechazar la hip√≥tesis
# nula, concluyendo as√≠ que no se puede rechazar que no hay diferencias en el porcentaje 
# de acierto alcanzado por los dos algoritmos de clasificaci√≥n, por lo que no se
# puede asegurar que hay un algoritmo mejor que otro.


# ------------------------------------------------------------------------------
# Pregunta 2
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las 
# lecturas dadas) en donde un estudio o experimento, relacionado con el alza que
# han experimentado las tasas de inter√©s de los cr√©ditos en Chile, necesite 
# utilizar una prueba de los rangos con signo de Wilcoxon, debido a problemas 
# con la escala de la variable dependiente en estudio. Indiqu√© cu√°les ser√≠an las
# variables/niveles involucrados en su ejemplo y las hip√≥tesis nula y 
# alternativa a contrastar.

# Una empresa de an√°lisis econ√≥mico desea comprobar si se percibe una diferencia 
# en las tasas de inter√©s (%) de los cr√©ditos hipotecarios que un banco entrega 
# a las personas luego del transcurso de los a√±os 2020 y 2021.

# Hip√≥tesis a contrastar:
# H0: No existen diferencias significativas en las tasas de inter√©s de los
#     cr√©ditos en Chile entre 2020 y 2021.
# HA: Existen diferencias significativas en las tasas de inter√©s de los
#     cr√©ditos en Chile entre 2020 y 2021.

# Variables:
# -Persona: (N√∫mero que representa a una persona)
# -A√±o: Periodo de tiempo, antes y despu√©s (2020 - 2021)
# -Tasa de interÈs de los crÈditos (%)

# Para este caso es preferible utilizar una prueba de los rangos con signo de 
# Wilcoxon debido a que existen valores at√≠picos, adem√°s de que el tama√±o de la
# muestra es peque√±o por lo que no se asegura normalidad.

# Los resultados se muestran a continuaci√≥n:
#
# Persona   2020    2021 
# 1         2%      3%  
# 2         1.5%    1.9%
# 3         2%      2.1%
# 4         3%      3.5%
# 5         2.5%    4.5%
# 6         1.5%    2%
# 7         2.9%    3%
# 8         2.5%    5.5%
# 9         2.5%    3%
# 10        3%      3.5%


# ------------------------------------------------------------------------------
# Pregunta 3
# El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos
# de clasificaci√≥n en diferentes conjuntos de prueba disponibles en el UCI 
# Machine Learning Repository. Los algoritmos corresponden a C3: averaged 
# one-dependence estimator (AODE), C6: locally weighted naive-Bayes y C7:
# random forest. ¬øExiste un algoritmo mejor o peor que los otros?

texto <- ("
 Dataset C3 C6 C7
 'pima-diabetes' 74.45 74.19 72.11
 'pendigits' 97.26 94.25 95.11
 'credit' 84.51 84.66 82.77
 'eucalyptus' 58.15 58.96 58.84
 'primary-tumor' 46.93 48.99 37.75
 'waveform' 84.36 83.06 79.12
 'solar-flare-X' 97.28 93.85 95.43
 'glass' 73.27 75.13 72.77
 'solar-flare-m' 87.36 86.43 84.90
 'hepatitis' 83.23 81.94 80.69
 'sonar' 80.70 80.23 77.80
 'page-blocks' 96.39 93.59 96.41
 'solar-flare-C' 87.98 87.36 85.49
 'yeast' 57.18 56.92 55.70
 'optdigits' 96.34 93.64 91.24
 'iris' 92.11 91.44 92.77
")
datos <- read.table(textConnection(texto), header = TRUE)

# Hip√≥tesis a contrastar:
# H0: No hay diferencias en el porcentaje de acierto alcanzado por los tres 
#   algoritmos de clasificaci√≥n
# HA: S√≠ hay diferencias en el porcentaje de acierto alcanzado por los tres 
#   algoritmos de clasificaci√≥n

# Se utiliza la prueba de Friedman puesto que se trabaja con tres muestras correlacionadas.
# Por lo que se verifican el cumplimiento de las condiciones:
# - La variable independiente es categ√≥rica y tiene tres niveles.
# - La escala de la variable dependiente es ordinal, puesto que se trata del acierto 
# alcanzado por tres algoritmos de clasificaci√≥n en diferentes conjuntos de prueba.
# - Se puede afirmar que los sujetos provienen de una muestra aleatoria e independiente
# de la poblaci√≥n.

# Prueba de Friedman 
n <- length(datos$C3)
Acierto <- c(datos$C3, datos$C6, datos$C7)
Algoritmo <- c( rep("C3", n),
                rep("C6", n),
                rep("C7", n))

Sujeto <- rep (1:n, 3)
Algoritmo <- factor(Algoritmo)
datos <- data.frame(Sujeto, Acierto, Algoritmo)
# Establecer nivel de significaci√≥n
alfa <- 0.05

# Hacer la prueba de Friedman .
prueba <- friedman.test(Acierto ~ Algoritmo|Sujeto, data = datos)
print(prueba)

# Conclusi√≥n de la prueba:
# Con respecto al resultado obtenido y con un valor de p = 0.00633, este siendo
# menor al nivel de significaci√≥n, por lo tanto, se rechaza la hip√≥tesis nula en
# favor de la alternativa concluyendo con un 95% de confianza que hay diferencias 
# en el porcentaje de acierto alcanzado por los dos algoritmos de clasificaci√≥n.

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias
# significativas .
if( prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$Acierto,
                                   datos$Algoritmo,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}
# Conclusi√≥n post-hoc de Holm
# Se utiliza esta prueba para buscar las diferencias significativas entre el
# porcentaje.
# Se concluye con un 95% de confianza que los pares de algoritmos 
# C7 - C3
# Tienen diferencias significativas en el porcentaje de acierto alcanzado
# en diferentes conjuntos de prueba.

# ------------------------------------------------------------------------------
#Pregunta 4
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las 
# lecturas dadas) en donde un estudio o experimento, relacionado con el alza que 
# han experimentado las tasas de inter√©s de los cr√©ditos en Chile, necesite 
# utilizar una prueba de Kruskal-Wallis, debido a problemas con la normalidad
# de los datos. Indiqu√© cu√°les ser√≠an las variables/niveles involucrados en su 
# ejemplo y las hip√≥tesis nula y alternativa a contrastar.

#Enunciado: 
#En los ˙ltimos aÒos se ha visto un incremento constante en la tasa de
#interÈs de los crÈditos bancarios. Por esta razÛn se quiere realizar 
#un estudio en relaciÛn con el alza de la tasa interÈs en los crÈditos 
#de 4 bancos diferentes en el transcurso de los aÒos, donde se 
#desea comparar el alza de interÈs que tuvo cada banco y verificar 
#si existe alguna diferencia en el alza de alguno de ellos. Para esto 
#se tomaron muestras en diferentes aÒos, donde se cumple que: 

# - La variable independiente, que corresponde a la tasa de interÈs de cada 
#   banco, tiene m·s de dos niveles ya que se muestrean m·s de dos aÒos en cada caso.
# - La escala de la variable es ordinal, ya que esta se ordena seg˙n el aÒo en que se muestrea.
# - Las observaciones son independientes entre sÌ.

# Variables:
# -Banco: (N˙mero que representa a un banco)
# -AÒo: Periodo de tiempo desde 2015 a 2020
# -Tasa de interÈs de los crÈditos (%)

#Las HipÛtesis a contrastar son:
#HipÛtesis Nula
#H0: En el transcurso de los aÒos, todos los bancos tuvieron alzas de 
#    interÈs similares.

#HipÛtesis Alternativa
#H1: Al menos un banco tuvo un comportamiento diferente en el alza de 
#    su tasa de interÈs.

#Como se tienen m·s de dos muestras y adem·s cada una de ellas es independiente 
#para realizar el estudio deseado es necesario utilizar la prueba de Kruskal-Wallis.

#Los datos de las muestras son los siguientes:

# AÒo    Banco 1    Banco 2    Banco 3    Banco 4
# 2015        2%       1.5%       2.5%       1.7% 
# 2016      2.2%       1.8%       2.5%         2%
# 2017      2.3%         2%       2.6%       2.1%
# 2018      2.5%       2.2%       2.7%       2.3%
# 2019        3%       2.7%       3.4%       2.8%
# 2020      3.6%       3.3%         4%       3.5%

