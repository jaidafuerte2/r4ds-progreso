#####################################
###                               ###
###         Capítulo 10           ###
###   Análisis exploratorio       ###
###          de datos             ###
###                               ###
#####################################

library(nycflights13)

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

########################
###
### 10.5.1.1 Ejercicios
###
########################

# 1. Utilice lo aprendido para mejorar la visualización de los 
# horarios de salida de los vuelos cancelados frente a los no 
# cancelados.
?nycflights13::flights
# dep_time es la hora de salida real
# sched_dep_time : es la hora de salida programada
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time), # dep_time es la hora de salida real
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time, y = after_stat(density))) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)
# podría usar after_stat(density para ver frecuencias relativas)
# en vez de valores absolutos. No me gusta mucho porque no es notorio
# que los vuelos cancelados son una minoría

# Mostrar cajas con las horas de salidas programadas y vuelos 
# cancelados o no
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = cancelled, y = sched_dep_time)) + 
  geom_boxplot()

# Muestra un gráfico de barras
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = cancelled)) + 
  geom_bar()
?after_stat()

# Gráfico de barras coloreado
nycflights13::flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = cancelled)) + 
  geom_bar(aes(fill = cancelled))

# 2. Según el EDA, ¿qué variable del conjunto de datos de diamantes 
# parece ser la más importante para predecir el precio de un 
# diamante? ¿Cómo se correlaciona esta variable con la talla? 
# ¿Por qué la combinación de estas dos relaciones hace que los
# diamantes de menor calidad sean más caros?
#
# Muestra el promedio de los precios según la calidad del diamante
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()
# Muestra el promedio de los precios según la calidad del diamante
#en orden
ggplot(diamonds, aes(x = fct_reorder(cut, price, median), y = price)) +
  geom_boxplot()
# Muestra el promedio de los precios según el peso (carat) del diamante
#en orden
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()
# El precio es proporcional al tamaño
#
# Muestra el promedio de los precios según el color del diamante
#en orden
ggplot(diamonds, aes(x = fct_reorder(color, price, median), y = price)) +
  geom_boxplot()
# D es el mejor color y tiene uno de los precios más bajos. J es el peor
# color y tiene uno de los precios más altos.

# Muestra el promedio de los precios según el color del diamante
#en orden
ggplot(diamonds, aes(x = fct_reorder(cut, price, median), y = carat)) +
  geom_boxplot()
# Parecería que los cortes de más baja calidad (fair) son para los 
# diamantes más pesados y los diamantes con cortes de mejor calidad
# son los más livianos. Es posible que significa que es más difícil
# cortar los diamantes más grandes

# 3. En lugar de intercambiar las variables x e y, se añade 
# coord_flip()una nueva capa al diagrama de caja vertical para 
# crear uno horizontal. ¿Cómo se compara esto con intercambiar 
# las variables?
# Con coord_flip()
ggplot(diamonds, aes(x = fct_reorder(cut, price, median), y = carat)) +
  geom_boxplot() +
  coord_flip()
# Intercambiando x e y
ggplot(diamonds, aes(x = carat, y = fct_reorder(cut, price, median))) +
  geom_boxplot()
# Respuesta: el gráfico es el mismo

# 4. Un problema con los diagramas de caja es que se desarrollaron 
# en una época con conjuntos de datos mucho más pequeños y tienden 
# a mostrar una cantidad prohibitiva de valores atípicos. Una 
# solución para solucionar este problema es el diagrama de valores 
# de letras. Instale el paquete lvplot e intente usarlo geom_lv() 
# para mostrar la distribución de precio vs. cut. ¿Qué aprende? 
# ¿Cómo interpreta los gráficos?
ggplot(diamonds, aes(x = fct_reorder(cut, carat, median), y = price)) +
  lvplot::geom_lv()
ggplot(diamonds, aes(x = cut, y = price)) +
  lvplot::geom_lv()
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()
# Parece que en algo podría ayudar a ver las proporciones de los valores
# atípicos

# 5. Cree una visualización de los precios de los diamantes frente 
# a una variable categórica del diamondsconjunto de datos usando 
# geom_violin(), luego un gráfico facetado geom_histogram(), luego 
# uno coloreado geom_freqpoly()y finalmente un gráfico coloreado 
# geom_density(). Compare y contraste los cuatro gráficos. ¿Cuáles 
# son las ventajas y desventajas de cada método para visualizar la 
# distribución de una variable numérica en función de los niveles 
# de una variable categórica?
#
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin()
# Realmente no entiendo para que se usa ni qué significa
#
# Se observa que los precios más altos son los más frecuentes
ggplot(diamonds, aes(x = price)) +
  geom_histogram()
#ggplot(diamonds, aes(x = cut)) + # no funciona
#  geom_histogram()
#
# Se observa que los precios más altos son los menos frecuentes
ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut))
# ayuda a comparar las cantidades por precios según los tipos de 
# cortes 
#
#Se observa que los más pesados son menos frecuentes
ggplot(diamonds, aes(x = carat)) +
  geom_freqpoly(aes(color = cut))
#
# En este gráfico se observa que los diamantes con peor corte son 
# los diamantes con un precio más estable pero barato
ggplot(diamonds, aes(x = price)) +
  geom_density(aes(color = cut))

# 6. Si tiene un conjunto de datos pequeño, a veces es útil 
# geom_jitter()evitar la sobregraficación para ver más fácilmente 
# la relación entre una variable continua y una categórica. 
# El paquete ggbeeswarm ofrece varios métodos similares a 
# geom_jitter(). Enumérelos y describa brevemente la función de 
# cada uno.
??ggbeeswarm
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_jitter()
ggplot(diamonds, aes(x = cut, y = price)) +
  ggbeeswarm::geom_quasirandom()
ggplot(diamonds, aes(x = cut, y = price)) +
  ggbeeswarm::geom_beeswarm()
View(mpg)
ggplot(mpg, aes(x = model, y = hwy )) +
  ggbeeswarm::geom_beeswarm()
ggplot(mpg, aes(x = model, y = hwy )) +
  geom_jitter()

# Mostrar la covariación entre variables categóricas. Para esto se debe
# contar el número de observaciones para cada combinación con la
# función geom_count()
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count(fill = n) # esto no funciona
?diamonds

# Calcular los recuentos
diamonds |>
  count(color, cut) # produce:
# A tibble: 35 × 3
#  color cut           n
#  <ord> <ord>     <int>
#1 D     Fair        163
#2 D     Good        662
#3 D     Very Good  1513
#4 D     Premium    1603
# Visualizar dos variables categóricas con geom_tile() lo que nos
# da una escala de colores fácil según el conteo
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

########################
###
### 10.5.2.1 Ejercicios
###
########################

# 1. ¿Cómo podría reescalar el conjunto de datos de recuento 
# anterior para mostrar más claramente la distribución del corte 
# dentro del color, o del color dentro del corte?
# Agrupado por color
diamonds |>
  count(color, cut) |>
  group_by(color) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))
# Agrupado por corte
diamonds |>
  count(color, cut) |>
  group_by(cut) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))

# 2. ¿Qué diferentes perspectivas de datos se obtienen con un 
# gráfico de barras segmentado si el color se asigna a la x 
# estética ? Calcule los recuentos que corresponden a cada 
# segmento. cutfill
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = n, fill = cut)) +
  geom_bar(stat = "identity")
# Perspectiva opuesta
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = cut, y = n, fill = color)) +
  geom_bar(stat = "identity") # que use el conteo de n que ya tiene
  # su columna propia porque lo calculé, de otra forma stat = "count"
  # por defecto y vuelve a  hacer su propio conteo
View(flights)
flights

# 3. Úsalo geom_tile()junto con dplyr para explorar cómo varían 
# los retrasos promedio en las salidas de vuelos según el destino 
# y el mes del año. ¿Qué dificulta la lectura del gráfico? 
# ¿Cómo podrías mejorarlo?
flights |>
  ggplot(aes(x = dest, y = month)) +
  geom_tile(aes(fill = dep_delay))
# Pienso que el problema es que month se toma como una variable 
# continua cuando es una categoría. Así que la solución que planteo
# es cambia a month de numérica a factor 
flights |>
  mutate(month = factor(month)) |>
  ggplot(aes(x = dest, y = month)) +
  geom_tile(aes(fill = dep_delay))
# De todas formas no funciona.
#
# Así que cambiaré la orientación:
flights |>
  mutate(month = factor(month)) |>
  ggplot(aes(x = month, y = dest)) +
  geom_tile(aes(fill = dep_delay))


View(smaller) # diamantes que son más pequeños que 3 quilates
# Diagrama de dispersión para ver la relación de peso con precio
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()
# Añadir transparencia al diagrama de dispersión para que no se 
# acumulen todos los puntos en ciertas zonas
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(alpha = 1/100)
# Muestra relación de peso y precio y también los cuenta
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()
# Parecido a geom_bin2d()
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()
# Agrupa una variable continua para que actúe como categórica
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

########################
###
### 10.5.3.1 Ejercicios
###
########################

# 1. En lugar de resumir la distribución condicional con un 
# diagrama de caja, se podría usar un polígono de frecuencias. 
# ¿Qué se debe considerar al usar cut_width()vs.  cut_number()? 
# ¿Cómo afecta esto a la visualización de la distribución 2D de 
# caraty price?
?geom_freqpoly()
?cut_number()

# 1️⃣ Filtrar dataset para simplificar
smaller <- diamonds %>% 
  filter(carat < 3)

# 2️⃣ Crear columna de bins usando cut_number
smaller <- smaller %>%
  mutate(carat_bin = cut_number(carat, 10))

  # 3️⃣ Graficar frequency polygon
ggplot(smaller, aes(x = price, color = carat_bin)) +
  geom_freqpoly()

# 2. Visualize the distribution of carat, partitioned by price.
ggplot(smaller, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_width(price, 1000)))

# 3. ¿Cómo se compara la distribución de precios de los diamantes 
# muy grandes con la de los diamantes pequeños? ¿Es como esperabas 
# o te sorprende?
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))
# En general no me sorprenden los valores. Sin embargo llama la 
# atención que los dos grupos más pesados no tienen los promedios de
# precio más altos. También veo que los diamantes más grandes tienen 
# precios atípicos por debajo de la media: en cambio los diamantes 
# no tan caros tienen precios atípicos por encima de la media. Tal vez
# no sea tan buen negocio vender diamantes grandes. Tal vez el negocio
# está en los pequeños.

# 4. Combine dos de las técnicas que ha aprendido para visualizar la 
# distribución combinada de corte, quilates y precio.
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/10)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d(aes(fill = cut))

# 5. Los gráficos bidimensionales revelan valores atípicos que no 
# son visibles en los gráficos unidimensionales. Por ejemplo, 
# algunos puntos del siguiente gráfico presentan una combinación 
# inusual de valores xy y, lo que los convierte en valores atípicos, 
# aunque sus valores xy yparezcan normales al examinarlos por 
# separado. ¿Por qué un diagrama de dispersión es una mejor 
# representación que un gráfico agrupado en este caso?
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point()

# 6. En lugar de crear cuadros de igual ancho con cut_width(), 
# podríamos crear cuadros que contengan aproximadamente el mismo 
# número de puntos con cut_number(). ¿Cuáles son las ventajas y 
# desventajas de este enfoque?
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 25))) # no puede ser
  # mayor a 25 porque no hay más de 25 bins. Si se usa algo como
  # 26 o 30 generará un error
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))
# No veo tantas ventajas en el enfoque de cut_number porque me parece
# que lo que hace es agrupar muchos casos de gran peso y precio; 
# mientras agrupa pocos casos de pequeño peso y precio, pero no
# veo que lo haga bajo una regla específica. En cambio con cut_width
# parece más fácil ver la diferencia de precios en cada segmento de
# peso.


