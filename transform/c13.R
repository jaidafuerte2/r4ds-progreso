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