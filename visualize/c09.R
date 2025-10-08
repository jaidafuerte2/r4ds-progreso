#####################################
###                               ###
###         Capítulo 9            ###
###            Capas              ### 
###                               ###
#####################################

library(tidyverse)

mpg # produce:
# A tibble: 234 × 11
#  manufacturer model     displ  year   cyl trans drv     cty   hwy fl   
#  <chr>        <chr>     <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#1 audi         a4          1.8  1999     4 auto… f        18    29 p    
#2 audi         a4          1.8  1999     4 manu… f        21    29 p    
#3 audi         a4          2    2008     4 manu… f        20    31 p    
#4 audi         a4          2    2008     4 auto… f        21    30 p    
# NOTA: el marco de datos mpg tiene 234 observaciones o filas sobre
# 38 modelos de automóviles
#View(mpg) # produce: a new window with dataframe

# Entre las variables de mpg tenemos:
# 1. displ: Cilindrada del coche en litros. Variable numérica
# 2. hwy: Eficiencia de combustible de un auto en carretera. En millas
#    por galón (mpg). Un auto con baja eficiencia de combustible 
#    consume más combustible al recorrer la misma distancia queun auto 
#    con alta eficiencia. Variable numérica
# 3. class: tipo de coche, es una variable categórica.

# Relación entre displ y hwy para varios class tipos de autos
ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point()
# Significa que un auto con mayor cilindrada (displ) tiene menor 
# eficiencia de combustible (hwy)
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) + 
  geom_point()
# Cuando class se asigna a shape se obtienen dos advertencias:
# 1: La paleta de formas puede manejar un máximo de 6 valores 
# discretos porque más de 6 se vuelve difícil de discriminar; usted 
# tiene 7.
# 2: Se eliminaron 62 filas que contenían valores faltantes 
# (geom_point()). Son 62 filas que se eliminaron porque contenían
# el séptimo tipo de auto, suv que no se grafica.

# Diagrama de dispersión con puntos de tamaños diferentes
ggplot(mpg, aes(x = displ, y = hwy, size = class)) + 
  geom_point() # produce:
# Using size for a discrete variable is not advised. 
# Diagrama de dispersión con puntos grises de opacidad distinta
ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) + 
  geom_point() # produce:
# Using alpha for a discrete variable is not advised. 

# Lograr que todos los puntos sean azules
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")
