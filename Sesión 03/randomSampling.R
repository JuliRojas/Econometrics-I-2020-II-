# ------------------------------- Script ----------------------------------
#                     Universidad Nacional de Colombia
#                     Facultad de Ciencias Económicas
#                              Econometría I
# ------------------------------ Monitoria --------------------------------
# 1. Hernando Hernandez Lasso.
# 2. Julián David Rojas Aguilar.

# ---------------------------- Instrucciones ------------------------------
# 1. Cada línea en el script se ejecuta con Ctrl + Enter o con el botón run de 
#    esta ventana.
# 2. Todo el texto que se encuentre después del numeral (#) es un comentario.
# 3. A la hora de ejecutar cada línea, R no tendrá en cuenta los comentarios.
# 4. Todos los resultados de comandos ejecutados se muestran en la consola.
# 5. Es posible ejecutar comandos directamente desde la consola con la tecla 
#    Enter.

# 1| Preparación ----------------------------------------------------------
# 1.1| Librerías ----------------------------------------------------------
rm(list = ls())
library(here)         # Define el directorio del proyecto.
library(readxl)       # Importar .xlsx.
library(readr)        # Importar .csv.

# 1.2| Bases de datos -----------------------------------------------------
rm(list = ls())
survey <- read_delim(here('input', 'Students Survey 2008.txt'), 
                     '\t', escape_double = FALSE, trim_ws = TRUE)

# 2| Muestreo aleatorio y pruebas de hipótesis ----------------------------
# Se crea unos datos aleatorios del 1 al 1325, el número de filas de la encuesta.
# - Replace = FALSE: Los números no se repiten. 
# - Se crea una submuestra con base en el resultado anterior.
set.seed(123)
muestra100 = sample(1:1325, 100, replace = FALSE)
surveySub = survey[muestra100,]

# 2.1| Comparación de distribuciones --------------------------------------
par(mfrow = c(2, 1))
hist(survey$ageinmonths, 
     main = 'Densidad de la edad \n Totalidad de los datos',
     sub  = 'Elaboración propia',
     xlab = 'Edad en meses',
     col  = '#E41A1C',
     freq = F) 
lines(density(survey$ageinmonths, adjust = 2)) 

hist(surveySub$ageinmonths, 
     main = 'Densidad de la edad \n Submuestra de los datos',
     sub  = 'Elaboración propia',
     xlab = 'Edad en meses',
     col  = '#E41A1C',
     freq = F) 
lines(density(surveySub$ageinmonths, adjust = 2)) 
par(mfrow = c(1, 1))

mean(survey$ageinmonths)
mean(surveySub$ageinmonths)

# 2.2| Pruebas de hipótesis -----------------------------------------------
# 2.2.1| A una cola -------------------------------------------------------
t.test(surveySub$ageinmonths, alternative = 'greater', mu = 235, 
       conf.level = 0.95)
(mean(surveySub$ageinmonths) - 235)/(sd(surveySub$ageinmonths)/sqrt(length(surveySub$ageinmonths)))
t.test(survey$ageinmonths, alternative = 'greater', mu = 235, conf.level = 0.95)

# 2.2.2| A dos colas ------------------------------------------------------
t.test(surveySub$ageinmonths, mu = 235, conf.level = 0.95)
t.test(survey$ageinmonths, mu = 235, conf.level = 0.95)

# 2.2.3| Sobre dos parámetros ---------------------------------------------
t.test(surveySub$ageinmonths[surveySub$gender=='female'],
       surveySub$ageinmonths[surveySub$gender=='male'], 
       alternative='two.sided')
