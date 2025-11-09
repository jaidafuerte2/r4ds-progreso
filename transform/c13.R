#####################################
###                               ###
###         Capítulo 13           ###
###           Números             ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

x <- c("1.2", "5.6", "1e3")
# parse_double() : convierte cadenas de texto a números
x # produce: [1] "1.2" "5.6" "1e3"
parse_double(x) # produce: [1]    1.2    5.6 1000.0
#
x <- c("$1,234", "USD 3,513", "59%")
x # produce: [1] "$1,234"    "USD 3,513" "59%" 
# parse_double() : convierte cadenas de texto (que contienen caracteres)
# no numéricos que se quieren ignorar) a números.
parse_number(x) # produce: [1] 1234 3513   59 

# Contar cuántos vuelos hay en cada destino 
flights |> count(dest) # produce:
# A tibble: 105 × 2
#  dest      n
#  <chr> <int>
#1 ABQ     254
#2 ACK     265
#3 ALB     439
#4 ANC       8
#
#  Contar cuántos vuelos hay en cada destino y mostrar los más comunes 
# con sort = TRUE 
flights |> count(dest, sort = TRUE) # produce:
# A tibble: 105 × 2
#dest      n
#  <chr> <int>
#1 ORD   17283
#2 ATL   17215
#3 LAX   16174
#4 BOS   15508

# Muestra el número de vuelos de cada destino. Además muestra un 
# promedio del retraso de llegada de los vuelos de cada destino
flights |> 
  group_by(dest) |> 
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )
# A tibble: 105 × 3
#  dest      n delay
#  <chr> <int> <dbl>
#1 ABQ     254  4.38
#2 ACK     265  4.85
#3 ALB     439 14.4 
#4 ANC       8 -2.5 
#
# n() es una función que solo funciona como argumento de funciones de
# orden superior. Por ejemplo una función n() suelta, no funciona
#n() # produce: error

# Muestra los vuelos agrupados por destino y el número de aerolíneas
# (carriers) que llevan a ese destino o tienen ese vuelo
flights |> 
  group_by(dest) |> 
  summarize(carriers = n_distinct(carrier)) |> 
  arrange(desc(carriers)) # produce:
# A tibble: 105 × 2
#dest  carriers
#  <chr>    <int>
#1 ATL          7
#2 BOS          7
#3 CLT          7
#4 ORD          7
# n_distinct(carrier) suma cuántas aerolíneas hay en cada vuelo (o 
# tienen la opción de cada vuelo.

# Agrupa los vuelos por el numero de placa (tailnum : plane tail 
# number) y suma la distancia que recorre en cada vuelo
flights |> 
  group_by(tailnum) |> 
  summarize(miles = sum(distance)) # produce:
#A tibble: 4,044 × 2
#tailnum  miles
#  <chr>    <dbl>
#1 D942DN    3418
#2 N0EGMQ  250866
#3 N10156  115966
#4 N102UW   25722
#
# Una forma abreviada del ejemplo anterior, es decir agrupar vuelos por 
# número de placa del avión y sumar la distancia que recorre un avión
# en cada vuelo; pero usando el argumento wt.
flights |> count(tailnum, wt = distance) # produce:
# A tibble: 4,044 × 2
#tailnum      n
#  <chr>    <dbl>
#1 D942DN    3418
#2 N0EGMQ  250866
#3 N10156  115966
#4 N102UW   25722

flights |> 
  group_by(dest) |> 
  summarize(n_cancelled = sum(is.na(dep_time))) |>
  arrange(desc(n_cancelled)) # produce:
# A tibble: 105 × 2
#  dest  n_cancelled
#  <chr>       <int>
#1 ORD           641
#2 DCA           548
#3 BOS           459
#4 RDU           367

########################
###
### 13.3.1 Ejercicios
###
########################

# 1. How can you use count() to count the number of rows with a missing value 
# for a given variable?
flights |>
  count(is.na(dep_time)) # produce:
# A tibble: 2 × 2
#  `is.na(dep_time)`      n
#  <lgl>              <int>
#1 FALSE             328521
#2 TRUE                8255
#
is.na(c(1, 2, NA, 3, 4, FALSE, NA)) # produce:
# [1] FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE
#
# Otra forma podría ser:
flights |>
  filter(is.na(dep_time)) |>
  count() # produce:
# A tibble: 1 × 1
#   n
#   <int>
#1  8255
#
flights |>
  filter(is.na(dep_time)) # produce:
# A tibble: 8,255 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA

# 2. Expand the following calls to count() to instead use group_by(), 
# summarize(), and arrange():
#flights |> count(dest, sort = TRUE)
#
#flights |> count(tailnum, wt = distance)
#
# Muestra el conteo de vuelos por destino y los ordena en forma 
# descendente
flights |> count(dest, sort = TRUE) # produce:
# A tibble: 105 × 2
#  dest      n
#  <chr> <int>
#1 ORD   17283
#2 ATL   17215
#3 LAX   16174
#4 BOS   15508
flights |>
  group_by(dest) |>
  summarize(n = n()) |>
  arrange(desc(n)) # produce:
# A tibble: 105 × 2
#  dest      n
#  <chr> <int>
#1 ORD   17283
#2 ATL   17215
#3 LAX   16174
#4 BOS   15508
#
# Muestra la distancia que recorre cada avión por su número de
# placa trasera
flights |> count(tailnum, wt = distance) # produce:
# A tibble: 4,044 × 2
# tailnum      n
#  <chr>    <dbl>
#1 D942DN    3418
#2 N0EGMQ  250866
#3 N10156  115966
#4 N102UW   25722
flights |>
  group_by(tailnum) |>
  summarize(n = sum(distance, na.rm = TRUE)) |> # na.rm = TRUE indica
  # que los valores faltantes deben ignorarse
  arrange(desc(n)) # produce:
# A tibble: 4,044 × 2
#  tailnum       n
#  <chr>     <dbl>
#1 NA      1784167
#2 N328AA   939101
#3 N338AA   931183
#4 N327AA   915665



x <- c(1, 2, 10, 20)
x / 5 # produce
#> [1] 0.2 0.4 2.0 4.0
# es una abreviatura de:
x / c(5, 5, 5, 5)
#> [1] 0.2 0.4 2.0 4.0

x * c(1, 2)
#> [1]  1  4 10 40
x * c(1, 2, 3)
#> Warning in x * c(1, 2, 3): longer object length is not a multiple of shorter
#> object length
#> [1]  1  4 30 20

# Es un error filtrar Enero y febrero de esta forma (porque se 
# intercalan las filas -pares e impares-):
flights |> 
  filter(month == c(1, 2)) # produce:
# A tibble: 25,977 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      542            540         2      923
#3  2013     1     1      554            600        -6      812
#4  2013     1     1      555            600        -5      913
# Lo correcto sería esto:
flights |> 
  filter(month == 1 | month == 2) # produce:
# A tibble: 51,955 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,
)
# Mostrar los valores mínimos  y máximos de una fila con pmin y pmax
df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )
# produce:
# A tibble: 3 × 4
#      x     y   min   max
#  <dbl> <dbl> <dbl> <dbl>
#1     1     3     1     3
#2     5     2     2     5
#3     7    NA     7     7
#
# pmin y pmax son distintos a min y max
df |> 
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  ) # produce:
# A tibble: 3 × 4
#      x     y   min   max
#  <dbl> <dbl> <dbl> <dbl>
#1     1     3     1     7
#2     5     2     1     7
#3     7    NA     1     7

# el clásico módulo de toda la vida
1:10 %% 3 # produce:
#[1] 1 2 0 1 2 0 1 2 0 1
# la clásica división entera:
1:10 %/% 3
#[1] 0 0 1 1 1 2 2 2 3 3
#
# Esto es útil, por ejemplo, para descomponer a sched_dep_time en 
# horas y minutos (porque por defecto sched_dep_time viene en algo
# como las 600 horas, que significa las 6:00)
flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  ) # produce:
# A tibble: 336,776 × 3
#  sched_dep_time  hour minute
#           <int> <dbl>  <dbl>
#1            515     5     15
#2            529     5     29
#3            540     5     40
#4            545     5     45

# Agrupo por hora de salida programada y luego saco la proporción
# de vuelos cancelados (vuelos cancelados sobre total de vuelos)
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n())
# produce:
# A tibble: 20 × 3
#   hour prop_cancelled     n
#  <dbl>          <dbl> <int>
#1     1        1           1
#2     5        0.00461  1953
#3     6        0.0164  25951
#4     7        0.0127  22821
#
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) # filtro por hora > 1
# produce:
# A tibble: 19 × 3
#   hour prop_cancelled     n
#  <dbl>          <dbl> <int>
#1     5        0.00461  1953
#2     6        0.0164  25951
#3     7        0.0127  22821
#4     8        0.0162  27242
#
# Muestro un diagrama de dispersión con la hora de salida programada
# en X y la proporción de vuelos cancelados en Y
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))

round(123.456) # produce: 123
round(123.456, 2) # produce: 123.46
round(123.456, 1) # produce: 123.5
round(123.456, 0) # produce: 123
round(123.456, -1) # produce: 120 # redondea a la decena más cercana
round(123.456, -2) # produce: 100 # redondea a la centena más cercana
round(123.456, -3) # produce: 0

round(c(1.5, 2.5)) # produce: 2 2 # esto es lo que se conoce como 
# redondeo al entero par o redondeo bancario. Es una buena práctica 
# porque porque mantiene el redondeo imparcial, la mitaad de .5's
# va para arriba y la otra mitad para abajo.

x <- 123.456
# floor() redondea hacia abajo
floor(x) # produce: 123
# ceiling() redondea hacia arriba
ceiling(x) # produce: 124

x <- c(1, 2, 5, 10, 15, 20)
# cut : divide o agrupa un vector en intervalos discretos:
cut(x, breaks = c(0, 5, 10, 15, 20)) # produce:
#[1] (0,5]   (0,5]   (0,5]   (5,10]  (10,15] (15,20]
#Levels: (0,5] (5,10] (10,15] (15,20]
#
# Los intervalos no necesitan estar espaciados uniformemente:
cut(x, breaks = c(0, 5, 10, 100)) # produce:
#[1] (0,5]    (0,5]    (0,5]    (5,10]   (10,100] (10,100]
#Levels: (0,5] (5,10] (10,100]
#
cut(x, breaks = c(0, 5, 10, 15, 20)) # produce:
#[1] (0,5]   (0,5]   (0,5]   (5,10]  (10,15] (15,20]
#Levels: (0,5] (5,10] (10,15] (15,20]
# Los levels son los intervalos
#
# Sin embargo los levels se pueden etiquetar
cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
) # produce:
#[1] sm sm sm md lg xl
#Levels: sm md lg xl
#
# Los valores por fuera del rango de los intervalos se convertirá
# en NA
y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20)) # produce:
#[1] <NA>   <NA>   (0,5]  (5,10] <NA>  
#Levels: (0,5] (5,10] (10,15] (15,20]

x <- 1:10
# cumsum() : acumula sumas
cumsum(x) # produce:
#[1]  1  3  6 10 15 21 28 36 45 55

########################
###
### 13.4.8 Ejercicios
###
########################

# 1. Explique con palabras qué hace cada línea del código 
# utilizado para generar la Figura  13.1 .
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))
#group_by(hour = sched_dep_time %/% 100) # Esta línea transforma 
# el sched_dep_time (La hora esperada de partida del avión) a horas
# (de 0 a 23 probablemente) puesto que las horas tienem un formato
# raro tipo 645 que significa 6:45. Entonces:
645 %/% 100 # produce: 6
786 %/% 100 # produce: 7
#summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) # lo que
# hace summarize() es crear dos nuevas variables en la tabla, 
# prop_cancelled que es la proporción de vuelos cancelados en una
# hora específica sobre el total de vuelos en una hora específica
# y n que es la cantidad de vuelos en una hora específica. Entonces:
flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n())
# produce:
# A tibble: 20 × 3
#   hour prop_cancelled     n
#  <dbl>          <dbl> <int>
#1     1        1           1
#2     5        0.00461  1953
#3     6        0.0164  25951
#4     7        0.0127  22821
#
#filter(hour > 1) : esta línea filtra los vuelos para que sean los
# mayores a 1 hora 
#
#ggplot(aes(x = hour, y = prop_cancelled)) + # crea la relación
#geom_line(color = "grey50") + # Crea una línea unida a cada punto
#geom_point(aes(size = n)) # Todo este código crea el diagrama de
# flujo

# 2. What trigonometric functions does R provide? Guess some 
# names and look up the documentation. Do they use degrees or 
# radians?
# Respuesta: tal vez : sin, cos, tan.
?Trig()
# Lo que dice la documentación es: Angles are in radians, not 
# degrees, for the standard versions

# 3. Actualmente, `a` dep_timey sched_dep_time`b` son convenientes 
# de ver, pero difíciles de calcular porque no son números 
# realmente continuos. Puede ver el problema básico ejecutando 
# el código a continuación: hay un espacio entre cada hora.
flights |> 
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()
#Conviértalos a una representación más precisa del tiempo (ya sean 
# horas fraccionarias o minutos desde la medianoche).
#
# Claro, hay un espacio entre 61 y 99 que no tiene vuelos porque
# los minutos se respresentan hasta 60. Lo que significa por ejemplo
# que nigún vuelo sale a las 675 porque no hay una hora 6:75.
#
flights |> 
  mutate(
    my_sched_dep_hour = sched_dep_time %/% 100,
    sched_dep_minutes = sched_dep_time %% 100,
    my_sched_dep_minutes = (sched_dep_minutes * 100) / 60,
    my_sched_dep_time = my_sched_dep_hour + my_sched_dep_minutes
  ) |>
  filter(month == 1, day == 1) |>
  ggplot(aes(x = my_sched_dep_time, y = dep_delay)) +
  geom_point()
  
# 4. Round dep_time and arr_time to the nearest five minutes.
flights |> 
  mutate(
    #my_sched_dep_hour = sched_dep_time %/% 100,
    #sched_dep_minutes = sched_dep_time %% 100,
    #my_sched_dep_minutes = (sched_dep_minutes * 100) / 60,
    #my_sched_dep_time = my_sched_dep_hour + my_sched_dep_minutes,
    my_dep_time = round(dep_time, -1),
    my_arr_time = round(arr_time, -1),
    .keep = "used"
  )



x <- c(1, 2, 2, 3, 4, NA)
# Asignar un valor de orden a números de un vector
min_rank(x) # produce:
#[1]  1  2  2  4  5 NA
#
x <- c(50, 10, 30, 10)
# En el caso de valores desordenados primero los ordena internamente
# y después los categoriza y devuelve esas categorías en el orden
# que se prensentaron al inicio
min_rank(x) # produce:
#[1] 4 1 3 1
#
x <- c(1, 2, 2, 3, 4, NA)
# los valores más pequeños obtienen los rangos más bajos
min_rank(desc(x)) # produce:
#[1]  5  3  3  2  1 NA
#
x <- c(5, 10, 10, 20)
min_rank(x)  # produce: [1] 1 2 2 4
dense_rank(x) # produce: [1] 1 2 2 3 # No deja huecos
row_number(x) # produce:  [1] 1 2 3 4 # Asigna números cosecutivos
# sin empates
#
df <- tibble(id = 1:10)
df # produce:
# A tibble: 10 × 1
#     id
#  <int>
#1     1
#2     2
#3     3
#4     4
#
df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  ) # produce:
# A tibble: 10 × 4
#     id  row0 three_groups three_in_each_group
#  <int> <dbl>        <dbl>               <dbl>
#1     1     0            0                   0
#2     2     1            1                   0
#3     3     2            2                   0
#4     4     3            0                   1

x <- c(2, 5, 11, 11, 19, 35)
# Mueve un lugar hacia adelante a todos los elementos de un vector
# iniciando con NA
lag(x)  # produce: [1] NA  2  5 11 11 19
x - lag(x) # produce : [1] NA  3  6  0  8 16
x == lag(x) # produce: [1]    NA FALSE FALSE  TRUE FALSE FALSE
# Mueve hacia atrás a todos los elementos de un vector terminando 
# con NA
lead(x) # produce: [1]  5 11 11 19 35 NA

# Registro de las horas que alguien visitó un sitio web
events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)
# Conocer cuánto tiempo pasó entre cada visita
events <- events |> 
  mutate(
    my_lag = lag(time),
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5
  )
events # produce:
# A tibble: 14 × 4
#   time  diff has_gap my_lag
#  <dbl> <dbl> <lgl>    <dbl>
#1     0     0 FALSE       NA
#2     1     1 FALSE        0
#3     2     1 FALSE        1
#4     3     1 FALSE        2
#
x <- 1:10
# cumsum() va cumulando la sumatoria de los números de un vector
cumsum(x) # produce:
#[1]  1  3  6 10 15 21 28 36 45 55
#
# Se podría agrupar
events |> mutate(
  group = cumsum(has_gap)
) # shoiuld produce:
# A tibble: 14 × 5
#   time  diff has_gap my_lag group
#  <dbl> <dbl> <lgl>    <dbl> <int>
#1     0     0 FALSE       NA     0
#2     1     1 FALSE        0     0
#3     2     1 FALSE        1     0
#4     3     1 FALSE        2     0
# NOTA: No entiendo bien el beneficio de esto
#
df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)
df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1) # produce:
# A tibble: 7 × 3
# Groups:   id [7]
#  x         y    id
#  <chr> <dbl> <int>
#1 a         1     1
#2 b         2     2
#3 c         4     3
#4 d         3     4
#5 e         9     5
#6 a         4     6
#7 b        10     7
#
# NOTA: No entiendo bien que hace esto, ni que utilidad puede tener


########################
###
### 13.5.4 Ejercicios
###
########################

# 1. Find the 10 most delayed flights using a ranking function. 
# How do you want to handle ties? Carefully read the documentation 
# for min_rank()
# Filtrar los NA en arr_delay
flights2 <- flights %>% filter(!is.na(arr_delay))
# usando min_rank: incluye todos los vuelos cuyo rango <= 10 (puede ser >10 filas)
top_with_ties_minrank <- flights2 %>%
  filter(min_rank(desc(arr_delay)) <= 10) %>%
  arrange(desc(arr_delay))
top_with_ties_minrank # produce:
# A tibble: 10 × 2
#  arr_delay first_ten
#      <dbl>     <int>
#1      1272         1
#2      1127         2
#3      1109         3
#4      1007         4
the_top <- top_with_ties_minrank |>
  mutate(
    first_ten = min_rank(desc(arr_delay)),
    .keep = "used"
  )
#View(the_top)
the_top # produce:
# A tibble: 10 × 2
#  arr_delay first_ten
#      <dbl>     <int>
#1      1272         1
#2      1127         2
#3      1109         3
#4      1007         4
#

# 2. Which plane (tailnum) has the worst on-time record? En español
# significa algo así como qué avión tiene el peor historial de 
# puntualidad?
# Entonces, qué significa on time? Puntualidad, traducido a r es:
#on_time = arr_delay <= 0 # o que el vuelo llega sin retraso
worst_on_time <- flights %>%
  group_by(tailnum) %>%
  summarize(
    prop_ontime = mean(arr_delay <= 0, na.rm = TRUE),  # proporción de vuelos a tiempo
    n = n()                                            # cuántos vuelos tiene el avión
  ) %>%
  filter(n > 20) %>%                                   # descartamos aviones con pocos vuelos
  arrange(prop_ontime)                                 # ordenamos de peor a mejor
worst_on_time # produce:
# A tibble: 3,138 × 3
# tailnum prop_ontime     n
#   <chr>         <dbl> <int>
#1 N988AT        0.2      37
#2 N983AT        0.25     32
#3 N980AT        0.255    47
#4 N969AT        0.265    34
#
worst_on_time <- flights %>%
  group_by(tailnum) %>%
  summarize(
    prop_ontime = mean(arr_delay <= 0, na.rm = TRUE), # proporción
    # de vuelos a tiempo
    n = n() # Número de vuelos (es decir numero de veces que aparece
            # cierta placa de avión -tailnum- en la tabla)                                            
  ) |> 
  arrange(prop_ontime) # arregla de manera ascendente dependiendo 
  # de los vuelos a tiempo
worst_on_time # produce:
# A tibble: 4,044 × 3
# tailnum prop_ontime     n
#   <chr>         <dbl> <int>
#1 N121DE            0     2
#2 N136DL            0     1
#3 N143DA            0     1
#4 N17627            0     2
#
worst_on_time %>% slice_head(n = 1) # produce:
# A tibble: 1 × 3
#   tailnum prop_ontime     n
#     <chr>         <dbl> <int>
#  1 N121DE            0     2
#
# slice_head() sirve para tomar las primeras filas de una tabla
df <- tibble(x = 1:10)
df # produce: 
# A tibble: 10 × 1
#      x
#  <int>
#1     1
#2     2
#3     3
#4     4
df |> slice_head(n = 3) # produce:
# A tibble: 3 × 1 
#      x
#  <int>
#1     1
#2     2
#3     3

# 3. What time of day should you fly if you want to avoid delays as 
# much as possible?
# Significa a qué hora del día hay menos retraso
better_hour <- flights |>
  mutate(dep_hour = dep_time %/% 100) |>
  group_by(dep_hour) %>%
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)                                         
  ) %>%
  arrange(avg_delay)                                
better_hour # produce: 
# A tibble: 26 × 2
#  dep_hour avg_delay
#     <dbl>     <dbl>
#1        4    -5.55 
#2        5    -4.36 
#3        6    -1.52 
#4        7     0.223
better_hour |> slice_head(n = 3) # produce:
# A tibble: 3 × 2
#  dep_hour avg_delay
#     <dbl>     <dbl>
#1        4     -5.55
#2        5     -4.36
#3        6     -1.52

# 4. What does 
# flights |> group_by(dest) |> filter(row_number() < 4) do? 
# What does 
# flights |> group_by(dest) |> filter(row_number(dep_delay) < 4) do?
?row_number()
flights |> group_by(dest) |> filter(row_number() < 4) # produce:
# A tibble: 311 × 19
# Groups:   dest [105]
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
other_flights <- flights |>
  mutate(
    dest = dest,
    dep_time = dep_time,
    my_row_number = row_number(),
    .keep = "used"
  ) |>
  group_by(dest) |>
  filter(row_number() < 4) # produce:
View(other_flights)
# A tibble: 311 × 3
# Groups:   dest [105]
#  dep_time dest  my_row_number
#     <int> <chr>         <int>
#1      517 IAH               1
#2      533 IAH               2
#3      542 MIA               3
#4      544 BQN               4
#
flights |> group_by(dest) |> filter(row_number(dep_delay) < 4) 
# produce:
# A tibble: 310 × 19
# Groups:   dest [104]
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      848            851        -3     1155
#2  2013     1     1      946            959       -13     1146
#3  2013     1     4     2140           2159       -19     2241
#4  2013     1     5      859            901        -2     1144
my_flights <- flights |>
  mutate(
    dest = dest,
    dep_delay = dep_delay,
    my_row_number = row_number(dep_delay),
    .keep = "used"
  ) |>
  group_by(dest) |>
  filter(row_number(dep_delay) < 4)
#View(my_flights)
# A tibble: 310 × 3
# Groups:   dest [104]
#  dep_delay dest  my_row_number
#      <dbl> <chr>         <int>
#1        -3 JAC          119045
#2       -13 AVL            1357
#3       -19 PWM              79
#4        -2 MTJ          143534
#
# Respuesta: 
#flights |> group_by(dest) |> filter(row_number() < 4)
# En este caso row_number enumera desagrupa pero a cada fila del 
# grupo la enumera. Algo así:
df <- tibble(
  dest = c("A", "A", "A", "B", "B", "C", "C", "C"),
  delay = c(5, 10, 20, 3, 15, 7, 9, 12)
)
df # produce: 
# A tibble: 8 × 2
#   dest  delay
#  <chr> <dbl>
#1 A         5
#2 A        10
#3 A        20
#4 B         3
df |>
  group_by(dest) |>
  mutate(rank = row_number()) # produce:
# A tibble: 8 × 3
# Groups:   dest [3]
#dest  delay  rank
#  <chr> <dbl> <int>
#1 A         5     1
#2 A        10     2
#3 A        20     3#
#
flights |> group_by(dest) |> filter(row_number(dep_delay) < 4)
# Esto no significa que se filtrarán los vuelos con retrasos menores
# a 4 minutos, significa que a cada dest se le agrupa y numera
# según el retraso y de esa numeráción se obtienen los menores a 4
# Ej:
df <- tibble(
  dest = c("A", "A", "A", "B", "B"),
  dep_delay = c(5, 12, 0, 20, 2)
)
#
df |> 
  group_by(dest) |> 
  mutate(rn = row_number()) # produce: 
# A tibble: 5 × 3
# Groups:   dest [2]
#  dest  dep_delay    rn
#  <chr>     <dbl> <int>
#1 A             5     1
#2 A            12     2
#3 A             0     3
#4 B            20     1
#5 B             2     2
#
df |> 
  group_by(dest) |> 
  mutate(rn = row_number(dep_delay)) # produce:
# A tibble: 5 × 3
# Groups:   dest [2]
#  dest  dep_delay    rn
#  <chr>     <dbl> <int>
#1 A             5     2
#2 A            12     3
#3 A             0     1
#4 B            20     2
#5 B             2     1
#
# Aquí vemos que desaparecen los rn menores a 3
df |> 
  group_by(dest) |> 
  mutate(rn = row_number(dep_delay)) |>
  filter(row_number(dep_delay) < 3) # produce:
# Groups:   dest [2]
#  dest  dep_delay    rn
#  <chr>     <dbl> <int>
#1 A             5     2
#2 A             0     1
#3 B            20     2
#4 B             2     1

# 5. For each destination, compute the total minutes of delay. 
# For each flight, compute the proportion of the total delay for 
# its destination. 
flights |> 
  group_by(dest) |> 
  mutate(
    total_delay_dest = sum(arr_delay, na.rm = TRUE),
    prop_delay = arr_delay / total_delay_dest
  ) |> 
  select(dest, flight, arr_delay, total_delay_dest, prop_delay)
# group_by(dest) : agrupa por destino
# total_delay_dest = sum(arr_delay, na.rm = TRUE) : crea una nueva
# variable que suma todos los retrasos de llegada
# prop_delay = arr_delay / total_delay_dest : calcula la proporción
# de retraso de cada vuelo relacionandolo con el total de retraso
# por destino
# select: selecciona las variables que se mostrarán.
#
# 6. Delays are typically temporally correlated: even once the 
# problem that caused the initial delay has been resolved, later 
# flights are delayed to allow earlier flights to leave. 
# Using lag(), explore how the average flight delay for an hour 
# is related to the average delay for the previous hour.
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) # produce:
# A tibble: 6,964 × 6
#   year month   day  hour dep_delay     n
#  <int> <int> <int> <dbl>     <dbl> <int>
#1  2013     1     1     5    -1.53     17
#2  2013     1     1     6    -1.16     51
#3  2013     1     1     7     1.38     37
#4  2013     1     1     8    14.1      64
#
# Lo que se quiere saber es si los retrasos se arrastran en el tiempo
# por ejemplo si en el aeropuerto a las 10 am hubo 20 minutos
# de retraso en promedio, ¿Es más probable que a las 11 am también
# haya un retraso?
#
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) |>
  # Aquí lag () mueve los valores una fila hacia atrás 
  mutate(prev_delay = lag(dep_delay)) |> 
  filter(!is.na(prev_delay)) # produce:
# A tibble: 6,751 × 7
#   year month   day  hour dep_delay     n prev_delay
#  <int> <int> <int> <dbl>     <dbl> <int>      <dbl>
#1  2013     1     1     6    -1.16     51     -1.53 
#2  2013     1     1     7     1.38     37     -1.16 
#3  2013     1     1     8    14.1      64      1.38 
#4  2013     1     1     9     6.02     52     14.1 
#
# Así que con el nuevo prev_delay se puede crear un gráfico para
# ver la relación
library(ggplot2)

flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) |> 
  mutate(prev_delay = lag(dep_delay)) |> 
  filter(!is.na(prev_delay)) |> 
  ggplot(aes(x = prev_delay, y = dep_delay)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Retrasos promedio por hora vs. retraso de la hora anterior",
    x = "Retraso promedio hora anterior (minutos)",
    y = "Retraso promedio actual (minutos)"
  )
# Entonces se ve una relación directamente proporcional y lineal
# entre las horas y el retraso de salida de los aviones. Es decir que
# los retrasos se acumulan, cuando una hora tiene retrasos altos,
# la siguiente también tiende a tenerlos

# 7. Look at each destination. Can you find flights that are 
# suspiciously fast (i.e. flights that represent a potential data 
# entry error)? Compute the air time of a flight relative to the 
# shortest flight to that destination. Which flights were most 
# delayed in the air?
#
# Paso 1: Calcular el tiempo relativo más corto
flights |> 
  group_by(dest) |> 
  mutate(
    min_air_time = min(air_time, na.rm = TRUE),
    rel_air_time = air_time - min_air_time,
    #.keep = "used"
  ) |> 
  ungroup() # se usa para desgrupar pues ya no se necesita que esté
            # agrupado
# produce:
# A tibble: 336,776 × 21
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
#
flights |> 
  group_by(dest) |> 
  mutate(
    min_air_time = min(air_time, na.rm = TRUE),
    rel_air_time = air_time - min_air_time,
    .keep = "used"
  ) # produce:
# A tibble: 336,776 × 4
# Groups:   dest [105]
#  dest  air_time min_air_time rel_air_time
#  <chr>    <dbl>        <dbl>        <dbl>
#1 IAH        227          161           66
#2 IAH        227          161           66
#3 MIA        160          125           35
#4 BQN        183          173           10
#
# Paso 3 : Encontrar vuelos sospechosamente rápidos o que tienen
# un rel_air_time muy bajo o negativo
flights |> 
  group_by(dest) |> 
  mutate(
    min_air_time = min(air_time, na.rm = TRUE),
    rel_air_time = air_time - min_air_time
  ) |> 
  ungroup() |> 
  filter(rel_air_time < 0 
         | rel_air_time < quantile(rel_air_time, 0.01, na.rm = TRUE)) |> 
  select(dest, flight, air_time, min_air_time, rel_air_time)
# quantile() : calcula percentiles que nos dicen el valor por debajo
# del cual cae cierto porcentaje de datos:
x <- c(1, 2, 3, 4, 5)
# El percentil 50 (o cuantil 0.5) es 3, es decir la mediana 
quantile(x, 0.5) # produce:
#50% 
#3 
# El percentil 90:
quantile(x, 0.9) # produce:
#90% 
#4.6 # Significa que el 90% de los valores de x son menores o
# iguales a 4.6
flights |>
  group_by(dest) |>
  summarize(
    q10 = quantile(air_time, 0.1, na.rm = TRUE),
    q50 = quantile(air_time, 0.5, na.rm = TRUE),
    q90 = quantile(air_time, 0.9, na.rm = TRUE),
  ) # produce:
# A tibble: 105 × 4
# dest    q10   q50   q90
# <chr> <dbl> <dbl> <dbl>
#1 ABQ    227   246   274 
#2 ACK     37    41    46 
#3 ALB     29    31    36 
#4 ANC    399.  414.  430.
#
# Paso 4: Identificar los 10 vuelos que más se demoraron en el aire
flights |> 
  group_by(dest) |> 
  mutate(
    min_air_time = min(air_time, na.rm = TRUE),
    rel_air_time = air_time - min_air_time
  ) |> 
  ungroup() |> 
  arrange(desc(rel_air_time)) |> 
  select(year, month, day, carrier, flight, dest, air_time, min_air_time, rel_air_time) |> 
  slice_head(n = 10) # produce:
# A tibble: 10 × 9
#   year month   day carrier flight dest  air_time min_air_time
#  <int> <int> <int> <chr>    <int> <chr>    <dbl>        <dbl>
#1  2013     7    28 DL         841 SFO        490          295
#2  2013    11    22 DL         426 LAX        440          275
#3  2013     1    28 AA         575 EGE        382          219
#4  2013     9    10 UA         745 DEN        331          182
#
# Paso 5: Interpretación
# - Si rel_air_time es negativo , hay probablemente errores en
# air_time
# - Si es muy grande podría reflejar: desvíos por mal clima, o
# congestión aérea, esperas en el aire antes de aterrizar o 
# simplemente rutas más largas por distintas aerolineas

# 8. Identifica todos los destinos operados por al menos dos 
# aerolíneas. Utiliza esos destinos para elaborar una 
# clasificación relativa de las aerolíneas según su desempeño en 
# cada destino.
#
# Paso 1: 
# - Filtrar los destinos (dest) que son servidos por dos o
# más aerolíneas (carrier)
# - Comparar el desempeño (por ejemplo el retraso promedio de llegada
# arr_delay) de esas aerolíneas solo dentro de los destinos que ambas 
# comparten
# - En resumen queremos ver si Delta llega más puntual que United
# en los destinos donde ambas vuelan
#
# Paso 2: Encontrar los destinos con al menos dos aerolíneas
multi_dest <- flights |>
  group_by(dest) |>
  summarize(n_carriers = n_distinct(carrier)) |>
  filter(n_carriers >= 2)
multi_dest # produce:
# A tibble: 76 × 2
#  dest  n_carriers
#  <chr>      <int>
#1 ATL            7
#2 AUS            6
#3 AVL            2
#4 BDL            2
#
# Paso 3: Calcular el rendimiento por aerolínea y servicio
carrier_rank <- flights |>
  filter(dest %in% multi_dest$dest) |> # nos quedamos solo con los
  # destinos  (dest) que tienen vuelos en 2 o más aerolíneas
  group_by(dest, carrier) |>
  #summarize(mean_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop_last") |>
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) |>
  mutate(rank = min_rank(mean_delay))
carrier_rank # produce:
# A tibble: 285 × 4
# Groups:   dest [76]
#  dest  carrier mean_delay  rank
#  <chr> <chr>        <dbl> <int>
#1 ATL   9E           0.857     1
#2 ATL   DL           7.42      3
#3 ATL   EV          19.6       6
#4 ATL   FL          20.7       7
# 
# min_rank() : ordena los valores de un vector y les asigna un ranking
x <- c(50, 20, 20, 10)
min_rank(x) # produce: [1] 4 2 2 1
# diferencia con row_number()
row_number(x) # produce: [1] 4 2 3 1
# row_number() asigna números según el orden original, sin respetar
# empates
#
# .groups = "drop_last" : desagrupa sólo un nivel. Es decir 
# desagrupó carrier pero sigue estando agrupado por dest. Si se quiere
# desgrupar todo, lo mejor es usar "drop" y no "drop_last"
#

