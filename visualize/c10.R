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

# Muestra que la única evidencia de valores atípicos son los límites 
# inusualmente amplios en el eje de las X
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)
# Acercar los valores pequeños del eje y con coord_cartesian() para
# facilitar la visualización de los valores inusuales
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))
# el parámetro ylim = c(0, 50) significa que se va a acercar los valores
# desde 0 hasta 50 en el eje de las Y. De esta forma ylim = c(0, 50)
# significa que se va a acercar los valores desde 0 a 30 en el eje 
# de las Y:
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0,20))

########################
###
### 10.3.3 Ejercicios
###
########################

# 1. Explora la distribución de cada una de las variables x, y, y z
# en diamonds. ¿Qué aprendes? Piensa en un diamante y cómo podrías 
# determinar qué dimensión es la longitud, el ancho y la profundidad.
ggplot(diamonds, aes(x = x)) +
  geom_histogram()
ggplot(diamonds, aes(x = y)) +
  geom_histogram()
ggplot(diamonds, aes(x = z)) +
  geom_histogram()
?diamonds
# Dice la documentación que x es largo, y es ancho y z es profundidad
# Observo que el largo es muy variable, el ancho no es tan variable
# y la profundidad tampoco es muy variable. No tengo suficiente
# conocimiento para saber por qué es así.

# 2. Explora la distribución de price. ¿Descubres algo inusual o 
# sorprendente? (Pista: Piensa detenidamente en binwidth y asegúrate 
# de probar con un amplio rango de valores).
ggplot(diamonds, aes(x = price)) +
  geom_histogram()
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 1000)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 400)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 300)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 200)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 100)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 50)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 20)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 10)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 5)
# A partir de 20 veo un tramo pequeño (pero visible) que no tiene
# diamantes para ese precio. Lo que a mi no me parece algo extraño
# pero no observo algo más atípico

# 3. ¿Cuántos diamantes son de 0,99 quilates? ¿Cuántos son de 1 
# quilate? ¿A qué crees que se debe la diferencia?

unusual <- diamonds |> 
  filter(carat == 0.99) |> 
  select(carat) |>
  arrange(carat)
unusual
# 0,99 tiene 13 filas
unusual <- diamonds |> 
  filter(carat == 1) |> 
  select(carat) |>
  arrange(carat)
unusual
# 1 tiene 1558 filas
# Mi parecer es que pienso que los que recogen los datos o los que pesan
# suelen redondear hacia arriba (tal vez).

# 4. Compara y contrasta coord_cartesian() con  xlim()el ylim() uso del 
# zoom en un histograma. ¿Qué ocurre si no se binwidth configura? 
# ¿Qué ocurre si intentas hacer zoom de forma que solo se muestre 
# la mitad de la barra?
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,5000))
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 20) +
  coord_cartesian(xlim = c(0,5000))
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 20) +
  coord_cartesian(xlim = c(1000,2000))
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 20) +
  coord_cartesian(xlim = c(1250,1750))  
# Ahora sí me parece sorprendente que de más de 50000 diamantes, ni
# uno sólo se vendió en 1500 dólares. No sé por qué pero me genera
# dudas.
# Si no se usa binwidth no se puede visualizar valores atípicos
ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  coord_cartesian(ylim = c(0,6000))
# Si acerco demasiado en el eje Y, no logro ver los valores 
# atípicos de Y.

ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(ylim = c(0,15))
# Reemplazar los valores inusuales por valores faltantes NA
diamonds2 <- diamonds |>
  mutate(y = if_else(y < 3 | y > 20, NA, y))
ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point()
# Para suprimir la advertencia "Warning message" uso el argumento
# na.rm
ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

# Crer una nueva variable y usar is.na() para ver si falta dep_time
# sched_dep_time es la hora de salida  que la cambia a un formato
# "común de horas" 
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

########################
###
### 10.4.1 Ejercicios
###
########################

# 1. ¿Qué sucede con los valores faltantes en un histograma? ¿Qué 
# sucede con los valores faltantes en un gráfico de barras? ¿Por qué 
# hay una diferencia en la gestión de los valores faltantes en 
# histogramas y gráficos de barras?
ggplot(diamonds2, aes(x = y)) +
  geom_histogram(binwidth = 0.009) #+
  #coord_cartesian(ylim = c(0,15))
ggplot(diamonds2, aes(x = y)) +
  geom_bar() #+
  #coord_cartesian(ylim = c(0,15))

# 2. ¿Qué na.rm = TRUE hace en mean()y sum()?
?mean()
# Dice la documentación lo siguiente:
# una evaluación lógica como VERDADERO o FALSO que indica si los 
# valores NA deben eliminarse antes de continuar con el cálculo.
# Es posible que TRUE significa que los valores desconocidos NA 
# sí deben eliminarse
?sum()
# Esto dice la documentación:
# Lógico (VERDADERO o FALSO). ¿Se deben eliminar los valores 
# faltantes (incluido NaN)?

# 3. Recrea la gráfica de frecuencias scheduled_dep_time coloreada 
# según si el vuelo se canceló o no. También faceta la cancelled
# variable. Experimenta con diferentes valores de la scales variable
# en la función de faceta para mitigar el efecto de más vuelos no 
# cancelados que cancelados.
#View(nycflights13::flights)
?nycflights13::flights
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) #+
  #facet_grid(~sched_dep_time, scales = "free") # Comenta este código
  # porque nycflights13::flights es una tabla supergrande y cuelga
  # el sistema
?facet_grid()

# Muestra la variación de una variable cateórica con una numérica,
# calidad de diamante (cut) vs precio (price)
ggplot(diamonds, aes(x = price)) +
  #geom_freqpoly(aes(color = cut))
  geom_freqpoly(aes(color = cut, binwidth = 500))

# Mostrar la densidad podría facilitar puede facilitar la comparación
# de diamantes según el tipo de corte del diamante. Para eso hay 
# que calcular la densidad con after_stat()
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  #geom_freqpoly(aes(color = cut))
  geom_freqpoly(aes(color = cut, binwidth = 500))

# Muestra el promedio de los precios según la calidad del diamante
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()
# Se obeserva que el promedio de los precios de los diamantes de baja
# calidad tienen precios más altos. Sorprendente!

# Muestra como varía el kilometraje en carretera por tipo de auto
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()
# Se puede reordenar class con fct_reorder() para que las cajas 
# estén reordenadas según el valor medio de hwy de cada tipo de auto
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()
# Cuando hay nombres de x muy largos se puede cambiar de orientación 
# al gráfico para que los nombres vayan en el eje Y. De esta forma:
ggplot(mpg, aes(x = hwy, y = fct_reorder(class, hwy, median))) +
  geom_boxplot()
