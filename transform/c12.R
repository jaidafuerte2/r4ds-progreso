#####################################
###                               ###
###         Capítulo 12           ###
###      Vectores lógicos         ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

# Mostrar en una pestaña nueva la tabla flights de nycflights13 
#View(flights)

x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2 # produce:
#[1]  2  4  6 10 14 22 26

df <- tibble(x)
df # produce:
#A tibble: 7 × 1
#      x
#    <dbl>
#1     1
#2     2
#3     3
#4     5

# Filtrar los vuelos que partieron después de las 6, antes de las 20
# y que tuvieron un retraso de llegada menor a 20 minutos
flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

# Crea nuevas columnas daytime y approx_ontime con datos booleanos 
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used" # controla qué columnas quedan en el resultado
    # en este caso quedan sólo las columnas usadas para el cálculo
    # y las nuevas porque se usó el argumento "used"
  ) # produce:
# A tibble: 336,776 × 4
#  dep_time arr_delay daytime approx_ontime
#     <int>     <dbl> <lgl>   <lgl>        
#1      517        11 FALSE   TRUE         
#2      533        20 FALSE   FALSE        
#3      542        33 FALSE   FALSE        
#4      544       -18 FALSE   TRUE   

# Crea nuevas columnas daytime y approx_ontime con datos booleanos
# Es parecido al anterior código
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime) # produce:
# A tibble: 172,286 × 21
#year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      601            600         1      844
#2  2013     1     1      602            610        -8      812
#3  2013     1     1      602            605        -3      821
#4  2013     1     1      606            610        -4      858

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x # produce: [1] 1 2
x == c(1, 2) # produce: FALSE
# NOTA: este resultado se produce porque los cálculos resultan en 
# aproximaciones que no son exactamente  iguales a los número 
# enteros en sí mismo. Por ejemplo:
print(x, digits = 16) # produce:
# [1] 0.9999999999999999 2.0000000000000004
# Es decir r redondea automaticamente estos números
# Qué hacer para comparar cantidades con deciamles tan pequeños?
# Usar la función near de dplyr
dplyr::near(x, c(1, 2)) # produce: [1] TRUE TRUE

# Las operaciones que involucran un valor desconocido, también serán 
# desconocidas
NA > 5 #  produce: NA
10 == NA #  produce: NA


# Añadiendo un poco de contexto, se entiende bien que las operaciones
# que incluyen valores faltantes, devuelven valores faltantes, ejemplo:
# Edad de mary desconocida
age_mary <- NA
# Edad de John desconocida
age_john <- NA
age_mary == age_john

# Este ejemplo falla (no filtra las filas con valores faltantes)
# porque dep_time == NA no devuelve un valor booleano TRUE o FALSE
# sino un valor NA, entonces no filtra nada
flights |> 
  filter(dep_time == NA) # produce:
# A tibble: 0 × 19
# ℹ 19 variables: year <int>, month <int>, day <int>, dep_time <int>,
#   sched_dep_time <int>, dep_delay <dbl>, arr_time <int>,
#   sched_arr_time <int>, arr_delay <dbl>, carrier <chr>, flight <int>,

# Para filtrar valores faltantes es mejor usar la función is.na()
is.na(c(TRUE, NA, FALSE)) # produce:
#[1] FALSE  TRUE FALSE
is.na(c(1, NA, 3)) # produce:
# [1] FALSE  TRUE FALSE
is.na(c("a", NA, "b")) # produce:
[1] FALSE  TRUE FALSE

# Entonces para encontrar filas con valores faltantes en la columna
# dep_time. el código sería el siguiente:
flights |> 
  filter(is.na(dep_time)) # produce:
# A tibble: 8,255 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA

# En este caso , con arrange(), los valores faltantes de la columna
# dep_time, van al final
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(dep_time)
# A tibble: 842 × 19
#year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
#
# Se filtra además por valores faltantes en dep_time, se ven cuatro
flights |> 
  filter(month == 1, day == 1, is.na(dep_time)) |> 
  arrange(dep_time) # produce:
# A tibble: 4 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA
# Se ordena en orden descendente anteponiendo los 4 valores faltantes
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time) # produce:
# A tibble: 842 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA
#5  2013     1     1      517            515         2      830
#6  2013     1     1      533            529         4      850

########################
###
### 12.2.4 Ejercicios
###
########################

# 1. ¿Cómo dplyr::near()funciona? Escribe nearpara ver el 
# código fuente. ¿Está sqrt(2)^2cerca del 2?
sqrt(2)^2 # produce: 2
sqrt(2)^2 == 2 # produce: FALSE
dplyr::near(sqrt(2)^2, 2) # produce: TRUE

# 2. Utilice mutate(), is.na(), y count()juntos para describir 
# cómo se conectan los valores faltantes en dep_time, 
# sched_dep_timey .dep_delay
flights |> 
  mutate(
    dep_time_na = is.na(dep_time)
  ) |>
  count(dep_time_na) # produce:
# A tibble: 2 × 2
#dep_time_na      n
#  <lgl>        <int>
#1 FALSE       328521
#2 TRUE          8255
#
flights_na_dep_time <- flights |> 
  mutate(
    dep_time_na = is.na(dep_time)
  )
flights_na_dep_time |>
  count(dep_time_na) # produce:
# A tibble: 2 × 2
#  dep_time_na      n
#  <lgl>        <int>
#1 FALSE       328521
#2 TRUE          8255
#
flights_na_sched_dep_time <- flights |> 
  mutate(
    sched_dep_time_na = is.na(sched_dep_time)
  )
flights_na_sched_dep_time |>
  count(sched_dep_time_na) # produce:
# A tibble: 1 × 2
#  sched_dep_time_na      n
#  <lgl>              <int>
#1 FALSE             336776
#
flights_na_dep_delay <- flights |> 
  mutate(
    dep_delay_na = is.na(dep_delay)
  )
flights_na_dep_delay |>
  count(dep_delay_na) # produce:
# A tibble: 2 × 2
#dep_delay_na      n
#  <lgl>         <int>
#1 FALSE        328521
#2 TRUE           8255
#
# RESPUESTA : MI parecer es que el retraso en la partida se relaciona
# o se calcula a partir de la hora de salida. En cambio la hora 
# programada de salida no se relaciona con la hora de salida (real).
# Por eso la hora de salida y el retraso en la hora de salida
# tienen 8255 valores faltantes. Sin embargo el retraso de la hora de
# salida también se calcula a apartir de la hora programada de 
# salida. Pero esto no afecta el número de valores faltantes en
# el retraso de la hora de salida porque el horario programado de
# salida no tiene valores faltantes. Lo que puede estar pasando es
# que un vuelo programado, simplemente, no se realizó.

df <- tibble(x = c(TRUE, FALSE, NA))
# El resultado de esta mutación recuuerda los resultados de lisp de
# las operaciones booleanas, tomando en cuenta que NA se considera
# un valor TRUE no FALSE. Entonces and devuelve el primer valor falso
# o el útimo verdadero. Y or devuelve el primer valor verdadero o el
# último falso
df |> 
  mutate(
    and = x & NA,
    or = x | NA
  )
# A tibble: 3 × 3
# x     and   or   
# <lgl> <lgl> <lgl>
#1 TRUE  NA    TRUE 
#2 FALSE FALSE NA   
#3 NA    NA    NA   
# De esta forma
TRUE & NA # produce: NA
FALSE & NA # produce: FALSE
NA & NA # produce: NA
TRUE | NA # produce: TRUE
FALSE | NA # produce: NA
NA | NA # produce: NA
NA & TRUE # produce: NA # Sin embargo esto falla con la lógica de 
# que NA es un valor # verdadero. En realidad NA es TRUE o FALSE 
# así que cuando va primero 
# en una operación booleana, el resultado siempre será NA.
