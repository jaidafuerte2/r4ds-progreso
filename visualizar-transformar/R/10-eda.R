##########################################################
##                                                      ##  
##                     10.- EDA                         ##
##                                                      ##
##########################################################

########################
##
## Introducción
##
########################

library(tidyverse)
library(kableExtra)

# Ver en forma general las variables de la tabla diamonds 
glimpse(diamonds[1:4,]) # produce:
#Rows: 4
#Columns: 10
#$ carat   <dbl> 0.23, 0.21, 0.23, 0.29
#$ cut     <ord> Ideal, Premium, Good, Premium
#$ color   <ord> E, E, E, I
#$ clarity <ord> SI2, SI1, VS1, VS2
#$ depth   <dbl> 61.5, 59.8, 56.9, 62.4
#$ table   <dbl> 55, 61, 65, 58
#$ price   <int> 326, 326, 327, 334
#$ x       <dbl> 3.95, 3.89, 4.05, 4.20
#$ y       <dbl> 3.98, 3.84, 4.07, 4.23
#$ z       <dbl> 2.43, 2.31, 2.31, 2.63

########################
##
##  Variación
##
########################

####### Valore Típicos #######

# Histograma que muestra la distribución de pesos (carat) de casi 54 mil 
# diamantes 
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

# Filtrar los diamantes más pequeños
smaller <- diamonds |> 
  filter(carat < 3)
# 
ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

####### Valores Atípicos #########

# Histograma del ancho del diamante (y) . y = ancho del diamante
ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5)

# Acercar los valores pequeños del eje y con coord_cartesian
ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

# Visualizar valores inusuales en la tabla de diamantes, filtrando por
# largo (x) y ancho(y) de los diamantes, seleccionando, largo, ancho y
# profundidad (z) del diamante y ordenando según el ancho
#?diamonds
unusual <- diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |>
  arrange(y)
unusual # produce:
# A tibble: 9 × 4
#  price     x     y     z
#  <int> <dbl> <dbl> <dbl>
#1  5139  0      0    0   
#2  6381  0      0    0   
#3 12800  0      0    0   
#4 15686  0      0    0   
#5 18034  0      0    0   
#6  2130  0      0    0   
#7  2130  0      0    0   
#8  2075  5.15  31.8  5.12
#9 12210  8.09  58.9  8.06

#############################
##
## Valores Inusuales
##
#############################

# Eliminar toda la fila con valores extraños (No recomendado)
diamonds2 <- diamonds |> 
  filter(between(y, 3, 20))

# Remplazar los valores inusuales con valores faltantes 
diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3 | y > 20, NA, y))

# Diagrama de dispersión que relaciona el largo con el ancho de un 
# diamante. 
ggplot(diamonds2, aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE) # Importante na.rm = TRUE para que no salga
# el mensaje de que se cambiaron los valores inusuales por valores 
# faltantes

#########################
##
## Covariación
##
#########################

####### Variable categórica y numérica #########

# Gráfico de frecuencia que muestra la variación de el precio según su 
# calidad (cut) de un diamante (difícil de interpretar)
ggplot(diamonds, aes(x = price)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

# Gráfico de densidad que muestra la varaición del precio según su
# calidad (un poco más fácil de interpretar)
ggplot(diamonds, aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

# Diagrama de caja que muestra la variación del precio según su calidad
# (fácil de interpretar)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

# Diagrama de caja que muestra la variación del hwy (millas por galón) 
# según la clase de auto. Pero las cajas ordenadas según la mediana de 
# su hwy
?mpg # prduce: hwy = millas por galón en carretera
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()
# El mismo gráfico girado 90 grados para que sea más fácil visualizar 
# los nombres que son largo
ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()

####### Dos variables numéricas #######

# Diagrama de dispersión que relaciona el peso (carat) con el precio de
# un diamante
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

# Diagrama de dispersión con transparencia que relaciona el peso 
# (carat) con el precio de un diamante. La transparencia es importante
# porque se visuliza la transposición de los puntos del diagrama
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_point(alpha = 1 / 100)

# Diagrama de caja que relaciona el precio del diamante con el precio
# como si el peso fuera una categoría gracias a group = cut_width(),
# esto agrupa (crea categorías) basadas en los valores numéricos de carat
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))
