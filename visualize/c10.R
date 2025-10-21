#####################################
###                               ###
###         Capítulo 9            ###
###   Análisis exploratorio       ###
###          de datos             ###
###                               ###
#####################################

# Dos tipos de preguntas siempre serán útiles para descubrir 
# información con tus datos. Estas preguntas se pueden formular, 
# en términos generales, como:
# 1. ¿Qué tipo de variación ocurre dentro de mis variables?
# 2. ¿Qué tipo de covariación ocurre entre mis variables?
#View(diamonds)
?diamonds
# carat : peso del diamante
# Muestra un histograma según pesos del diamante
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5) # binwidth define el ancho de x

# Es bueno buscar cosas inesperadas con estas 3 preguntas:
# 1. ¿Cuáles son los valores más comunes? ¿Por qué?
# 2. ¿Qué valores son raros? ¿Por qué? ¿Se ajustan a tus expectativas?
# 3. ¿Ves algún patrón inusual? ¿Qué podría explicarlo?
# Se filtran los diamantes con caret menor a 3 porque casi no hay con
# cafret mayor a 3
smaller <- diamonds |>
  filter(carat < 3)
# # Muestro el comportamiento del gráfico en segmentos pequeños de X 
ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# En este gráfico se observa que hay más diamantes en quilates enteros
# y en fracciones comunes de quilates. También se observa que hay más
# diamantes ligeramente a la derecha de cada pico
