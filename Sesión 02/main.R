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


# Dólar y petróleo --------------------------------------------------------
# 1| Preparación ----------------------------------------------------------
# 1.1| Librerías ----------------------------------------------------------
library(here)         # Define el directorio del proyecto.
library(readxl)       # Importar .xlsx.
library(readr)        # Importar .csv.
library(e1071)        # Estimación de los momentos.
library(carData)      # Bases de datos adicionales.
library(tidyverse)    # Manipulación de datos y generación de gráficos.
library(lubridate)    # Manipulación de fechas.
library(RColorBrewer) # Colores. 

# 1.2| Bases de datos -----------------------------------------------------
dollar <- read_csv(file.choose(),
                   locale = locale(decimal_mark = ",", grouping_mark = "."))
dollar <- read_csv(here('input', 'dollarBanrep.csv'),
                   locale = locale(decimal_mark = ",", grouping_mark = "."))
Brent  <- read_csv(here('input', 'BrentInvesting.csv'),
                   locale = locale(decimal_mark = ",", grouping_mark = "."))

# 1.3| Manipulación de datos ----------------------------------------------
# 1.3.1| Arreglo de los datos ---------------------------------------------
dollar           <- dollar[, c(2,4)]
colnames(dollar) <- c('Fecha', 'TRM')
dollar$Fecha     <- ymd(dollar$Fecha)

Brent           <- Brent[nrow(Brent):1, 1:2]
colnames(Brent) <- c('Fecha', 'Brent')

# Arreglo de las fechas en 'Brent':
inicialDate    = ymd('1991-12-01')
numberOfMonths = nrow(Brent)
Brent['Fecha'] = data.frame(Fecha = inicialDate + months(0:(numberOfMonths-1)) - days(1))
Brent$Fecha    = ymd(Brent$Fecha)

# 1.3.2| Unión de las bases -----------------------------------------------
firstDataset <- inner_join(x = dollar, y = Brent, by = 'Fecha')
rm(list = c('Brent', 'dollar', 'inicialDate', 'numberOfMonths'))


# 2| Análisis -------------------------------------------------------------
attach(firstDataset)

# 2.1| Gráficas -----------------------------------------------------------
# Para los colores:
display.brewer.all()
brewer.pal(3, 'Set1')

par(mfrow = c(2, 1))
plot(x = Fecha, y = TRM, 
     main = 'Evolución de la TRM a través del tiempo',
     sub  = 'Elaboración propia, con datos extraídos del Banco de la República.',
     col  = '#E41A1C',
     type = 'l')
plot(x = Fecha, y = Brent, 
     main = 'Evolución del precio de la referencia Brent a través del tiempo',
     sub  = 'Elaboración propia, con datos extraídos de Investing.',
     col  = '#377EB8',
     type = 'l')
par(mfrow = c(1, 1)) 

plot(x = TRM, y = Brent, 
     main = 'Diagrama de dispersión entre el precio de la referencia Brent y la TRM',
     sub  = 'Elaboración propia, con datos extraídos del Banco de la República e Investing.',
     col  = '#4DAF4A',
     pch  = 20)

# 2.2| Estadísticos -------------------------------------------------------
# 2.2.1| Asociación -------------------------------------------------------
cor(x = TRM, y = Brent, method = 'pearson')
cov(x = TRM, y = Brent)

# 2.2.2| Dispersión -------------------------------------------------------
var(TRM)
var(Brent)

sd(TRM)
sd(Brent)

# 2.2.3| Otras ------------------------------------------------------------
mean(TRM)
quantile(TRM) # Media > Mediana.
              # Asimétrica hacia la derecha o positiva.
range(TRM)
IQR(TRM)


# 3| Regresión ------------------------------------------------------------
summary(lm(TRM ~ Brent))


# Encuestas ---------------------------------------------------------------
rm(list = ls())
survey <- read_delim(here('input', 'Students Survey 2008.txt'), 
                     '\t', escape_double = FALSE, trim_ws = TRUE)
View(survey)

head(survey)     
head(survey, 10) 
names(survey)

# 1| Análisis -------------------------------------------------------------
summary(survey) 
summary(survey$sleep)

# 1.1| Gráficas -----------------------------------------------------------
table(survey$gender)
table(survey$gender, survey$hand)

# 1.1.1| Diagramas de barras ----------------------------------------------
barplot(table(survey$gender), 
        main = 'Histograma del sexo', 
        sub  = 'Elaboración propia',
        xlab = 'Sexo', 
        ylab = 'Número de personas',
        col  = c('#E41A1C','#377EB8'))

# 1.1.2| Diagramas de barras segmentados ----------------------------------
barplot(table(survey$gender, survey$hand), 
        main = 'Histograma segmentado (Género - Escritura)',
        sub  = 'Elaboración propia',
        xlab = 'Sexo', 
        ylab = 'Número de personas',
        col  = c('#E41A1C','#377EB8'))
legend('topleft', c('Mujer', 'Hombre'), col = c('#E41A1C', '#377EB8'), pch = 15)

# 1.1.3| Gráfico circular -------------------------------------------------
pie(table(survey$eyecolor), edges = 10000, radius = 1)


# 1.2| Estadísticos -------------------------------------------------------
# 1.2.1| Tendencia central ------------------------------------------------
mean(ageinmonths)
median(ageinmonths)
summary(ageinmonths)

# 1.2.2| Dispersión -------------------------------------------------------
kurtosis(ageinmonths)
skewness(ageinmonths)

range(ageinmonths)
IQR(ageinmonths)
sd(ageinmonths)
var(ageinmonths)

# A| Histograma -----------------------------------------------------------
hist(survey$ageinmonths, 
     main = 'Histograma de la edad',
     sub  = 'Elaboración propia',
     xlab = 'Edad en meses',
     col  = '#377EB8')

# B| Densidad -------------------------------------------------------------
hist(survey$ageinmonths, 
     main = 'Densidad de la edad',
     sub  = 'Elaboración propia',
     xlab = 'Edad en meses',
     col  = '#E41A1C',
     freq = F)
lines(density(survey$ageinmonths))

# C| Diagrama de caja -----------------------------------------------------
boxplot(survey$ageinmonths, 
        main = 'Diagrama de caja de la edad',
        sub  = 'Elaboración propia',
        ylab = 'Edad en meses',
        col  = '#E41A1C')