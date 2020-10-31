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

# Preparación -------------------------------------------------------------
rm(list = ls())
library(here)
library(readr)
library(tidyverse)
library(gdata)
library(stargazer)

# Bases de datos ----------------------------------------------------------
generalCharacteristics <- read_delim(here('input', 'Características generales.csv'), 
                                     ';', escape_double = FALSE, trim_ws = TRUE)
workersCharacteristics <- read_delim(here('input', 'Características ocupados.csv'), 
                                     ';', escape_double = FALSE, trim_ws = TRUE)

# 1| Manipulación ---------------------------------------------------------
# 1.1| Creación de los ID -------------------------------------------------
#      Se procede a cambiar los nombres de las variables consideradas como
#      apropiadas para la creación de las identificaciones, a saber:
#      - DIRECTORIO: ID.Vivienda
#      - ORDEN: ID.Persona
#      - SECUENCIA_P: ID.Hogar
# Para las Características Generales: A través de 
colnames(generalCharacteristics)[which(colnames(generalCharacteristics) == 'DIRECTORIO')] <- 'ID.Vivienda'
colnames(generalCharacteristics)[which(colnames(generalCharacteristics) == 'ORDEN')] <- 'ID.Persona'
colnames(generalCharacteristics)[which(colnames(generalCharacteristics) == 'SECUENCIA_P')] <- 'ID.Hogar'
# Para los Individuos Ocupados:
workersCharacteristics <- rename.vars(workersCharacteristics,
                                      from = c('DIRECTORIO', 'ORDEN', 'SECUENCIA_P'),
                                      to = c('ID.Vivienda', 'ID.Persona', 'ID.Hogar'))
# Luego, el ID único vendrá dado por:
generalCharacteristics = generalCharacteristics %>% 
  add_column(ID = paste(as.character(generalCharacteristics$ID.Vivienda), 
                        as.character(generalCharacteristics$ID.Hogar),
                        as.character(generalCharacteristics$ID.Persona), 
                        sep = ''),
             .before = 1)
workersCharacteristics = workersCharacteristics %>% 
  add_column(ID = paste(as.character(workersCharacteristics$ID.Vivienda), 
                        as.character(workersCharacteristics$ID.Hogar),
                        as.character(workersCharacteristics$ID.Persona), 
                        sep = ''),
             .before = 1)
# Se verifica que las llaves sean únicas:
length(generalCharacteristics$ID) == length(unique(generalCharacteristics$ID))
length(workersCharacteristics$ID) == length(unique(workersCharacteristics$ID))

# 1.2| Unión de las bases de datos ----------------------------------------
#      Se revisa los nombres que son idénticos en ambas bases de datos y se 
#      deja, solamente, los de 'generalCharacteristics', pues es el
#      documento más extenso. Posee a todos los individuos.
intersect(names(generalCharacteristics), names(workersCharacteristics))
workersCharacteristics = select(workersCharacteristics, -c('ID.Vivienda', 
                                                           'ID.Hogar', 
                                                           'ID.Persona', 
                                                           'HOGAR', 'REGIS', 
                                                           'AREA', 'MES', 
                                                           'DPTO', 
                                                           'fex_c_2011'))
# Cualquiera de las uniones de los datos listadas a continuación es válida.
dataset = inner_join(x = generalCharacteristics, 
                     y = workersCharacteristics, 
                     by = 'ID')
dataset = merge(x = generalCharacteristics, 
                y = workersCharacteristics, 
                by = 'ID')

# 1.3| Selección de las variables -----------------------------------------
#      Finalmente, se selecciona las variables a trabajar, a saber:
#      -   p6800: Horas que trabaja a la semana.
#      -   p6850: Horas trabajadas la semana pasada.
#      -   p7045: Horas trabajadas en una labor secundaria a la semana.
#      - inglabo: Ingresos laborales mensuales.
#      -   p6040: Edad.
#      -     esc: Escolaridad.
#      -   p6020: Sexo.
dataset = select(dataset, 
               c('P6800', 'P6850', 'P7045', 'INGLABO', 'P6040', 'ESC', 'P6020'))
dataset = select(dataset, c('P6800', 'INGLABO', 'P6040', 'ESC', 'P6020'))
colnames(dataset) <- c('Trabajo semanal', 'Ingreso mensual', 
                       'Edad', 'Escolaridad', 'Sexo')

# Hay dos opciones para trabajar con el sexo, a saber:
# 1| Factores: Trabajemos con esta.
dataset = dataset %>% mutate(Sexo = case_when(Sexo == 1 ~ 'Hombre', Sexo == 2 ~ 'Mujer'),
                             Sexo = factor(Sexo, levels = c('Hombre', 'Mujer')))
# 2| Binarias: 
#    dataset = dataset %>% mutate(Sexo = case_when(Sexo == 1 ~ 1, Sexo == 2 ~ 0))

# Se procede a generar las variables de salario por hora y experiencia potencial.
dataset = dataset %>%
  add_column(`Salario por hora` = (dataset$`Ingreso mensual`*12)/(dataset$`Trabajo semanal`*52),
             `Experiencia Potencial` = dataset$Edad - dataset$Escolaridad - 5)
# Pueden analizarse un poco los datos, por ejemplo:
max(dataset$`Salario por hora`, na.rm = T)
min(dataset$`Salario por hora`, na.rm = T)
max(dataset$Escolaridad, na.rm = T)
min(dataset$Escolaridad, na.rm = T)

# Para no operar con logaritmo valores de cero (0), el logaritmo del salario
# irá con un 'case_when', como se mostraba previamente.
dataset = dataset %>% 
  mutate(`Ln(Salario por hora)` = case_when(`Ingreso mensual` > 1000  ~ log(`Salario por hora`), 
                                            `Ingreso mensual` <= 1000  ~ NA_real_))
# Se revisa cuántos datos se encontraban por debajo del umbral.
sum(is.na(dataset$`Ln(Salario por hora)`))
length(dataset$`Ln(Salario por hora)`) - sum(is.na(dataset$`Ln(Salario por hora)`))

# Se desecha los valores perdido:
dataset = as.data.frame(drop_na(dataset))

# 2| Regresión ------------------------------------------------------------
#    Ya con todas las variables necesarias creadas, se procede a realizar la
#    estimación.
originalRegression <- lm(`Ln(Salario por hora)` ~ Escolaridad + 
                           `Experiencia Potencial` + 
                           I(`Experiencia Potencial`^2), data = dataset)
summary(originalRegression)

discriminationRegression <- lm(`Ln(Salario por hora)` ~ Escolaridad + 
                                 `Experiencia Potencial` + 
                                 I(`Experiencia Potencial`^2) +
                                 Sexo, data = dataset)
summary(discriminationRegression)

# 2.1| Presentación -------------------------------------------------------
#      Un poco de estadística descriptiva podría ser:
stargazer(dataset, type = 'text', title = 'Estadística descriptiva', digits = 2)

# Así mismo, las diferencias entre las regresiones:
stargazer(originalRegression, discriminationRegression, type = 'text',
          title = 'Comparación de las regresiones', digits = 2)

# 3| Supuestos sobre el modelo --------------------------------------------
# Las nuevas librerías serán:
library(reshape2)    # Para acomodar los datos a ggplot en el BoxPlot.
library(outliers)    # Para detectar valores atípicos.
library(psych)       # Distribución de las variables.
library(GGally)      # Gráficos de correlación.
library(corrplot) 
library(strucchange) # Permite comprobar la existencia de un cambio estructural.
library(car)         # Permite hacer las pruebas de hipótesis sobre un conjunto de parámetros.
library(lmtest)      # Pruebas sobre modelos lineales.
library(mctest)      # Prueba de hipótesis sobre multicolinealidad.
options(scipen = 999)# Sin notación científica.

# 3.1| Visualización de los datos -----------------------------------------
# A| Cajas ----------------------------------------------------------------
# Los datos son llevados a un lenguaje entendible por ggplot:
dataset = dataset %>%
  add_column(ID = 1:dim(dataset)[1],
             .before = 1)
Box = melt(dataset, id = c('ID','Sexo')) 
Box = Box %>% filter(variable %in% c('Trabajo semanal', 'Ingreso mensual', 
                                         'Edad', 'Escolaridad'))

ggplot(data = Box, aes(x = factor(1), y = value, colour = Sexo)) +
  geom_boxplot() + facet_wrap(. ~ variable, scales = 'free') +
  labs(title    = 'Gráfica de caja por variable', 
       subtitle = 'Comparación entre sexos',
       x        = '',
       y        = '',
       caption  = 'Elaboración propia. \n Datos extraídos del DANE.') + 
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        axis.text.x           = element_blank(),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# B| Impacto de la educación en el salario --------------------------------
dataset = dataset %>%
  add_column(Nada         = if_else(dataset$Escolaridad >=  0, 1, 0),
             Primaria     = if_else(dataset$Escolaridad >=  5, 1, 0),
             Secundaria   = if_else(dataset$Escolaridad >=  8, 1, 0),
             Bachillerato = if_else(dataset$Escolaridad >= 11, 1, 0),
             Pregrado     = if_else(dataset$Escolaridad >= 16, 1, 0),
             Posgrado     = if_else(dataset$Escolaridad >= 18, 1, 0))

dataset = dataset %>%
  mutate(Logros = case_when(Posgrado     == 1 ~ 'Posgrado',
                            Pregrado     == 1 ~ 'Pregrado',
                            Bachillerato == 1 ~ 'Bachillerato',
                            Secundaria   == 1 ~ 'Secundaria', 
                            Primaria     == 1 ~ 'Primaria',
                            Nada         == 1 ~ 'Nada'),
         Logros = factor(Logros, 
                         levels = c('Posgrado', 'Pregrado', 'Bachillerato', 
                                    'Secundaria', 'Primaria', 'Nada')))

ggplot(dataset, aes(x = Escolaridad, y = `Ln(Salario por hora)`, colour = Logros)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(title    = 'Modelo minceriano', 
       subtitle = 'Comparación por logros educativos',
       x        = 'Años de educación',
       y        = 'Ln(Salario por hora)',
       caption  = 'Elaboración propia. \n Datos extraídos del DANE.') + 
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# C| Impacto de la experiencia en el salario ------------------------------
ggplot(dataset, aes(x = `Experiencia Potencial`, y = `Ln(Salario por hora)`, colour = Sexo)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(Logros ~ .) +
  labs(title    = 'Modelo minceriano', 
       subtitle = 'Comparación por logros educativos',
       x        = 'Experiencia potencial \n Medida en años',
       y        = 'Ln(Salario por hora)',
       caption  = 'Elaboración propia. \n Datos extraídos del DANE.') + 
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# 3.2| Manejo de datos atípicos -------------------------------------------
# Diagnóstico:
par(mfrow = c(3, 2)) # Entorno recomendado para el diagnóstico.
plot(discriminationRegression, which = 1:6)
par(mfrow = c(1, 1)) # Se regresa al entorno típico.

# Implementamos el algoritmo: Basado en z-score.
OS.TrabajoSemanal <- scores(dataset$`Trabajo semanal`)
OS.IngresoMensual <- scores(dataset$`Ingreso mensual`)
OS.Edad           <- scores(dataset$Edad)
OS.Escolaridad    <- scores(dataset$Escolaridad)

dataset$Atípico   <- OS.TrabajoSemanal > 3 | OS.TrabajoSemanal < -3 | OS.IngresoMensual > 3 | OS.IngresoMensual < -3 | OS.Edad > 3 | OS.Edad < -3 | OS.Escolaridad > 3 | OS.Escolaridad < -3
newDataset        <- dataset %>% filter(Atípico == F)
newBox            <- melt(newDataset, id = c('ID', 'Sexo')) 
newBox            <- newBox %>% filter(variable %in% c('Trabajo semanal', 
                                                       'Ingreso mensual', 
                                                       'Edad', 'Escolaridad'))
newBox$value      <- as.numeric(newBox$value)

ggplot(data = newBox, aes(x = factor(1), y = value, colour = Sexo)) +
  geom_boxplot() + facet_wrap(. ~ variable, scales = 'free') +
  labs(title    = 'Gráfica de caja por variable', 
       subtitle = 'Comparación entre sexos',
       x        = '',
       y        = '',
       caption  = 'Elaboración propia. \n Datos extraídos del DANE.') + 
  theme_bw() +
  theme(plot.title            = element_text(size = 12, face = 'bold'),
        plot.subtitle         = element_text(face = 'italic'),
        axis.text.x           = element_blank(),
        legend.position       = 'bottom',
        legend.box.background = element_rect(),
        legend.box.margin     = margin(6, 6, 6, 6))

# 3.3| Forma funcional adecuada -------------------------------------------
options(scipen = 0) # Con notación científica.

# El daño viene de no incluir los rendimientos decrecientes de la experiencia:
damagedRestrictedRegression <- lm(`Salario por hora` ~ Escolaridad + 
                                    `Experiencia Potencial` + Sexo, data = dataset)
dataset$`Ajustado cuadrado` <- fitted(damagedRestrictedRegression)^2
dataset$`Ajustado cúbico`   <- fitted(damagedRestrictedRegression)^3

damagedRegression <- lm(`Salario por hora` ~ Escolaridad + 
                          `Experiencia Potencial` + `Ajustado cuadrado` +
                          `Ajustado cúbico` + Sexo, data = dataset)

linearHypothesis(damagedRegression, c('`Ajustado cuadrado`=0', '`Ajustado cúbico`=0'))
resettest(damagedRestrictedRegression, power = 2:3, type = 'fitted')

# El daño viene de no incluir el logaritmo natural del salario:
damagedRestrictedRegression <- lm(`Salario por hora` ~ Escolaridad + 
                                    `Experiencia Potencial` + 
                                    I(`Experiencia Potencial`^2) + 
                                    Sexo, data = dataset)
dataset$`Ajustado cuadrado` <- fitted(damagedRestrictedRegression)^2
dataset$`Ajustado cúbico`   <- fitted(damagedRestrictedRegression)^3

damagedRegression <- lm(`Salario por hora` ~ Escolaridad + 
                          `Experiencia Potencial` + I(`Experiencia Potencial`^2) +
                          `Ajustado cuadrado` + `Ajustado cúbico` + Sexo, 
                        data = dataset)

linearHypothesis(damagedRegression, c('`Ajustado cuadrado`=0', '`Ajustado cúbico`=0'))
resettest(damagedRestrictedRegression, power=2:3, type='fitted')

# Se corrige el último problema y se testea nuevamente:
damagedRestrictedRegression <- lm(`Ln(Salario por hora)` ~ Escolaridad +
                                    `Experiencia Potencial` + 
                                    I(`Experiencia Potencial`^2) +
                                    Sexo, data = dataset)
dataset$`Ajustado cuadrado` <- fitted(damagedRestrictedRegression)^2
dataset$`Ajustado cúbico`   <- fitted(damagedRestrictedRegression)^3

damagedRegression <- lm(`Ln(Salario por hora)` ~ Escolaridad +
                          `Experiencia Potencial` + I(`Experiencia Potencial`^2) +
                          `Ajustado cuadrado` + `Ajustado cúbico` + Sexo, 
                        data = dataset)

linearHypothesis(damagedRegression, c('`Ajustado cuadrado`=0', '`Ajustado cúbico`=0'))
resettest(damagedRestrictedRegression, power = 2:3, type = 'fitted')

# 3.4| Sin cambios estructurales ------------------------------------------
CUSUM <- efp(discriminationRegression, data = dataset, type = 'OLS-CUSUM')
plot(CUSUM, alpha = 0.05, 
     main = 'CUSUM', 
     xlab = 'Ln(Salario por hora)', 
     ylab = 'Proceso de fluctuación empírico')
sctest(`Ln(Salario por hora)` ~ Escolaridad + `Experiencia Potencial` + 
         I(`Experiencia Potencial`^2) + Sexo, order.by = Escolaridad, 
       data = dataset, type = 'Chow', point = 1000)

CUSUM <- Fstats(discriminationRegression, data = dataset)
plot(CUSUM, alpha = 0.05,
     main = 'Maximización del estadístico', 
     xlab = 'Ln(Salario por hora)', 
     ylab = 'Estadístico F')
lines(breakpoints(CUSUM))

# 3.5| No colinealidad perfecta -------------------------------------------
originalVariables <- dataset[, 2:5]
ggpairs(originalVariables)
corrplot.mixed(cor(originalVariables), 
               tl.col = 'black', 
               upper  = 'square', 
               tl.pos = 'lt', 
               diag   = 'l')
vif(discriminationRegression)

omcdiag(discriminationRegression)
imcdiag(discriminationRegression)