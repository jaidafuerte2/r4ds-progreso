##########################################################
##                                                      ##  
##             12.- Vectores Lógicos                    ##
##                                                      ##
##########################################################

#############################
##
## Introducción
##
#############################

library(tidyverse)
library(nycflights13)

#############################
##
## Comparaciones
##
#############################

# Filtrar la tabla flights por salidas diurnas que llegan 
# aproximadamente a tiempo
my_flights <- flights %>%
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)
my_flights[1:6,] # produce:
# A tibble: 6 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      601            600         1      844
#2  2013     1     1      602            610        -8      812
#3  2013     1     1      602            605        -3      821
#4  2013     1     1      606            610        -4      858
#5  2013     1     1      606            610        -4      837
#6  2013     1     1      607            607         0      858

# Crear variables lógicas(boleanas) de los que salieron en la mañana
# y otra variable de los que llegaron más o menos a tiempo. Luego
# filtrar según estas nuevas variables lógicas
my_flights <- flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used" # Mostrar sólo las variables que se usaron en mutate
  ) |>
  filter(daytime & approx_ontime)
my_flights[1:6,] # produce:
# A tibble: 6 × 4
#  dep_time arr_delay daytime approx_ontime
#     <int>     <dbl> <lgl>   <lgl>        
#1      601        -6 TRUE    TRUE         
#2      602        -8 TRUE    TRUE         
#3      602        16 TRUE    TRUE         
#4      606       -12 TRUE    TRUE         
#5      606        -8 TRUE    TRUE         
#6      607       -17 TRUE    TRUE 

################ Valores Faltantes #####################

# Los valores faltantes son contagiosos, una comparación con un
# valor faltante, dará como resultado un valor faltante 
NA > 5 # produce:
#> [1] NA
10 == NA # produce:
#> [1] NA

# We don't know how old Mary is
age_mary <- NA

# We don't know how old John is
age_john <- NA

# Are Mary and John the same age?
age_mary == age_john
#> [1] NA
# We don't know!

# En conclusión, el siguiente código no funciona:
flights |> 
  filter(dep_time == NA)

# Para obtener u resultado adecuado se necesita is.na()

################### is.na() #####################

is.na(c(TRUE, NA, FALSE)) # produce:
#> [1] FALSE  TRUE FALSE
is.na(c(1, NA, 3)) # produce:
#> [1] FALSE  TRUE FALSE
is.na(c("a", NA, "b")) # produce:
#> [1] FALSE  TRUE FALSE

# Filtra a todas las observaciones con que tienen valores faltantes 
# en la variable dep_time
my_flights <- flights |> 
  filter(is.na(dep_time))
my_flights[1:6,]
# A tibble: 6 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA
#5  2013     1     2       NA           1540        NA       NA
#6  2013     1     2       NA           1620        NA       NA

#############################
##
## álgebra booleana
##
#############################

############# Orden de las operaciones #################

# Forma correcta de filtrar a los vuelos de Noviembre y Diciembre
my_flights <- flights |> 
  filter(month == 11 | month == 12)
my_flights[1:6,]
# A tibble: 6 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013    11     1        5           2359         6      352
#2  2013    11     1       35           2250       105      123
#3  2013    11     1      455            500        -5      641
#4  2013    11     1      539            545        -6      856
#5  2013    11     1      542            545        -3      831
#6  2013    11     1      549            600       -11      912

# Forma incorrecta de filtrar a los vuelos de Noviembre y Diciembre
my_flights <- flights |> 
  filter(month == 11 | 12)
my_flights[1:6,]
# A tibble: 6 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013    11     1        5           2359         6      352
#2  2013    11     1       35           2250       105      123
#3  2013    11     1      455            500        -5      641
#4  2013    11     1      539            545        -6      856
#5  2013    11     1      542            545        -3      831
#6  2013    11     1      549            600       -11      912

############## %in% #############

# %in% devuelve un vector lógico de la misma longitud que x que es 
# TRUE siempre que un valor en x esté en cualquier lugar de y.
1:12 %in% c(1, 5, 11) # produce:
#>  [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  
#>  TRUE FALSE
letters[1:10] %in% c("a", "e", "i", "o", "u") # produce:
#>  [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE

# Para encontrar todos los vuelos de noviembre y diciembre 
# podríamos escribir:
my_flights <- flights |> 
  filter(month %in% c(11, 12))
my_flights[1:6,]
# A tibble: 6 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013    11     1        5           2359         6      352
#2  2013    11     1       35           2250       105      123
#3  2013    11     1      455            500        -5      641
#4  2013    11     1      539            545        -6      856
#5  2013    11     1      542            545        -3      831
#6  2013    11     1      549            600       -11      912

# Con %in% las reglas de NA son diferentes
c(1, 2, NA) == NA # produce:
#> [1] NA NA NA
c(1, 2, NA) %in% NA # produce:
#> [1] FALSE FALSE  TRUE

# Por ejemplo, este código filtra los valores de deptime que son
# NA o 0800
flights |> 
  filter(dep_time %in% c(NA, 0800))

#############################
##
## Resúmenes
##
#############################

###########  Resúmenes numéricos de vectores lógicos ############

# Cuando se utiliza un vector lógico en un contexto numérico, 
# TRUE se convierte en 1 y FALSE se convierte en 0. Esto hace que 
# sum() y mean()sean muy útiles con vectores lógicos porque sum(x)
# da el número de TRUEs y mean(x) da la proporción de TRUEs. 
# (porque mean() simplemente usa sum() para sumar y divide por 
# length())

# Sacar la proporción de vuelos que se retrasaron en la salidad
# como máximo una hora y el número de vuelos  que se retrasaron 
# en la llegada cinco o más horas
my_flights <- flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop" # desagrupa
  )
my_flights[1:6,] # produce:
# A tibble: 6 × 5
#   year month   day proportion_delayed count_long_delay
#  <int> <int> <int>              <dbl>            <int>
#1  2013     1     1              0.939                3
#2  2013     1     2              0.914                3
#3  2013     1     3              0.941                0
#4  2013     1     4              0.953                0
#5  2013     1     5              0.964                1
#6  2013     1     6              0.959                0

#############################
##
## Transformaciones 
## Condicionales 
##
############################# 

############### if_else() #################

x <- c(-3:3, NA)
# if_else ejecuta una comparación, si es verdadera se ejecuta el
# segundo argumento del paréntesis, sino el tercero. En el caso de
# valores faltantes devuelve NA
if_else(x > 0, "+ve", "-ve") # produce:
#[1] "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" NA  

# Existe un cuarto argumento opcional que se usará si la entrada es NA
if_else(x > 0, "+ve", "-ve", "???") # produce:
#> [1] "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" "???"

############### case_when() ###################

x <- c(-3:3, NA)
# Case when relaiza varias comparaciones, cuando encuentra una TRUE,
# devuelve lo que está posterior a "~"
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???"
) # produce:  
# [1] "-ve" "-ve" "-ve" "0"   "+ve" "+ve" "+ve" "???"

# Se puede usar .default para crear un valor predeterminado que
# abarque todos los casos.
case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
) # produce:
# [1] "-ve" "-ve" "-ve" "???" "+ve" "+ve" "+ve" "???"

