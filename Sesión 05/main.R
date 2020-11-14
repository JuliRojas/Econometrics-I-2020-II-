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
library(here)      # Referencias relativas.
library(foreign)   # Importar archivos de Stata.
library(tidyverse) # Manipulación de datos y generación de gráficas.
library(AER)       # Estimación por variables instrumentales.
library(arm)
library(lmtest)    
library(estimatr)  # Estimación por variables instrumentales con errores estándar robustos.
library(stargazer) # Presentación de las regresiones.

# Bases de datos ----------------------------------------------------------
# De las 753 observaciones, las primeras 428 son mujeres con horas positivas 
# trabajadas en 1975, mientras que las 325 mujeres restantes no trabajaron
# a cambio de una remuneración económica en 1975.
data   = read.dta(file = 'http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta')
dataIV = data %>% subset(!is.na(wage))

# 1| Problemas de endogeneidad --------------------------------------------
MCO = lm(log(wage) ~ educ, data = dataIV)
summary(MCO)

# 1.1| Relevancia ---------------------------------------------------------
#      Instrumento: Educación del padre de la mujer.
Beta = with(dataIV, cov(log(wage), fatheduc)/cov(educ, fatheduc))
Beta

summary(lm(log(educ) ~ fatheduc, data = dataIV))

# 2| Mínimos cuadrados en dos etapas --------------------------------------
# 2.1| Forma manual -------------------------------------------------------
# 2.1.1| Regresión auxiliar (Primera etapa) -------------------------------
#      Se incluyen como controles todas las variables exógenas del modelo.
#      Se evalúa la relevancia del instrumento.
FS = lm(educ ~ fatheduc, data = dataIV)
summary(FS)

FSIV = fitted(FS)

# 2.1.2| Regresión final (Segunda etapa) ----------------------------------
manualIV = lm(log(wage) ~ FSIV, data = dataIV)
summary(manualIV)

# 2.2| Forma automática ---------------------------------------------------
#      La fórmula requiere:
#      endógena ~ endógena + exógenas | instrumentos + exógenas.
automaticIV = ivreg(log(wage) ~ educ | fatheduc, data = dataIV)
summary(automaticIV)

automaticRIV = iv_robust(log(wage) ~ educ | fatheduc, data = dataIV)
summary(automaticRIV)

# 3| Resultados -----------------------------------------------------------
stargazer(MCO, manualIV, automaticIV, type = 'text',
          column.labels = c('MCO', 'MC2E', 'IV'),
          title = 'Comparación de las regresiones', digits = 2)

# 4| Extensión: Variables control y dos instrumentos ----------------------
# 4.1| MCO ----------------------------------------------------------------
MCO = lm(log(wage) ~ educ + exper + I(exper^2) + city, data = dataIV)
summary(MCO)

# 4.2| Relevancia ---------------------------------------------------------
cor(dataIV$educ, dataIV$motheduc)
cor(dataIV$educ, dataIV$fatheduc)

summary(lm(educ ~ exper + I(exper^2) + city + fatheduc + motheduc, 
           data = dataIV))

# 4.3| Forma manual -------------------------------------------------------
# 4.3.1| Regresión auxiliar (Primera etapa) -------------------------------
FS = lm(educ ~ exper + I(exper^2) + city + fatheduc + motheduc, 
        data = dataIV)
summary(FS)

FSIV = fitted(FS)

# 4.3.2| Regresión final (Segunda etapa) ----------------------------------
manualIV = lm(log(wage) ~ fitted(FS) + exper + I(exper^2) + city, 
              data = dataIV)
summary(manualIV)

# 4.3.3| Forma automática -------------------------------------------------
automaticIV = ivreg(log(wage) ~ educ + exper + I(exper^2) + city | motheduc +
                      fatheduc + exper + I(exper^2) + city, data = dataIV)
summary(automaticIV)

# 4.3.4| Resultados -------------------------------------------------------
stargazer(MCO, manualIV, automaticIV, type = 'text',
          column.labels = c('MCO', 'MC2E', 'IV'),
          title = 'Comparación de las regresiones', digits = 2)

# 5| Prueba de endogeneidad (Hausman) -------------------------------------
#    H0: Exogeneidad.
#    ¿Es necesario emplear variables instrumentales? Argumento teórico, pero
#    puede hallarse evidencia desde la estadística. De no haber endogeneidad,
#    la estimación vía MCO es mucho más eficiente.

# 5.1| Regresión aumentada ------------------------------------------------
#      Variable dependiente vs. exógenas (originales), endógena y residuales 
#      de la primera etapa.

residualsFS = resid(FS)
augmented   = lm(log(wage) ~ residualsFS + educ + exper + I(exper^2) + city, 
                 data = dataIV)
summary(augmented)

# Interpretación: Si el coeficiente que acompaña los residuos estimados en la 
# primera etapa es estadísticamente significativo, se rechaza la hipótesis
# nula de exogeneidad.

# Intuición: La parte endógena del eegresor queda almacenada en los residuales
# de la primera etapa.

# 5.2| Diagnóstico --------------------------------------------------------
#      - Los instrumentos son fuertes.
#      - Exogeneidad.
#      - H0: Los instrumentos son exógenos.
#        HA: Al menos uno de los instrumentos es endógeno (sobreidentificación).
diagnosis <- iv_robust(log(wage) ~ educ + exper + I(exper^2) + city | motheduc +
                         fatheduc + exper + I(exper^2) + city, data = dataIV, 
                       diagnostics = TRUE)
summary(diagnosis, diagnostics = TRUE)

# Aclaración: Los supuestos básicos son relevancia y exogeneidad. El primero
# es comprobable, el segundo no. Este último se argumenta desde la teoría
# económica.