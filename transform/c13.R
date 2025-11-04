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
