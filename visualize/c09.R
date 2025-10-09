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

########################
###
### 9.2.1 Ejercicios
###
########################

# 1. Crea un diagrama de dispersión de hwyvs  displdonde los puntos 
# estén rellenos de rosa en los triángulos.
ggplot(mpg, aes(x = hwy, y = displ)) + 
  geom_point(shape = "triangle", color = "pink")

# 2. ¿Por qué el siguiente código no generó un gráfico con puntos azules?
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))
# El color específico debe establecerse por fuera del mapeo estético
# Debe ser algo como: 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

# 3. ¿Qué strokehace la estética? ¿Con qué formas funciona? (Pista: 
# usa ?geom_point)
?geom_point
# Parecería que stroke controla el ancho de los puntos
ggplot(mpg, aes(x = displ, y = hwy, stroke = 0.5)) +
  geom_point()

# 4. ¿Qué ocurre si asignas una estética a algo distinto del nombre 
# de una variable, como aes(color = displ < 5)? Ten en cuenta que 
# también deberás especificar x e y.
ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) +
  geom_point()
# Es interesante que si se le asigna una condición a una estética,
# esta estética cambia según la categoría TRUE y FALSE

# Crear un diagrama de dispersión 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
# Crear una linea suave
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
# Crear un diagrama de dispersión y una línea suave
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth()

# Separar las líneas suaves según la variable drv (tipo de tracción)
ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth()
# Lo que resulta un poco extraño

# Separar las líneas suaves según la variable drv (tipo de tracción)
# Y mostrarlas como tiós de líneas diferentes
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) +
  geom_smooth()
# Aquí ya aparece el tipo de transimisión f para transmisión delantera,
# (o front) , r para transmisión trasera, y 4 para transmisión en 
# las 4 llantas

# Se puede clarificar con el siguiente gráfico
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))
# Esto también sería aceptable
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(shape = drv))
# Pero sería lo mismo que no poner nada en geom_smooth
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth()

# Se puede agrupar e distintas líneas suaves
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))
# El problema con la estética group es que no pone leyendas así que
# no se sabe a que nos referimos
#Por eso se podría agrupar con la estética linetype
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(linetype = drv))
# Aquí vemos que ya se pueden ver las leyendas del tipo de tracción
# f, r y 4
# También la estética color es una buena opción si se necesita ver las 
# leyendas:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))
