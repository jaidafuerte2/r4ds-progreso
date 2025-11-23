#####################################
###                               ###
###         Capítulo 18           ###
###           Valores             ###
###        Faltantes (NA)         ###
###                               ###
#####################################

library(tidyverse)

# Cuando los datos se introducen manualmente, los valores faltantes 
# a veces indican que el valor de la fila anterior se ha repetido 
# (o transferido):
treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)
treatment # produce:
# A tibble: 4 × 3
#  person           treatment response
#  <chr>                <dbl>    <dbl>
#1 Derrick Whitmore         1        7
#2 NA                       2       10
#3 NA                       3       NA
#4 Katherine Burke          1        4

# Para completar estos valores faltantes se puede usar tidyr::fill() .
treatment |>
  fill(everything())
#> # A tibble: 4 × 3
#>   person           treatment response
#>   <chr>                <dbl>    <dbl>
#> 1 Derrick Whitmore         1        7
#> 2 Derrick Whitmore         2       10
#> 3 Derrick Whitmore         3       10
#> 4 Katherine Burke          1        4

# Algunas veces los valores faltantes representan un valor fijo 
# y conocido, normalmente 0. Para rremplazarlos se puede usar la
# función dplyr::coalesce() .
x <- c(1, 4, 5, 7, NA)
coalesce(x, 0) # produce:
#> [1] 1 4 5 7 0

# NaN significa not a number y se comporta similar a un NA
x <- c(NA, NaN)
x * 10 # produce:
#> [1]  NA NaN
x == 1 # produce:
#> [1] NA NA
is.na(x) # produce:
#> [1] TRUE TRUE

# NaN puede encontrarse en casos de operciones matemáticas de 
# resultado inesperado
0 / 0 # produce:
#> [1] NaN 
# NOTA: Inf es infinito
0 * Inf # produce:
#> [1] NaN
Inf - Inf # produce:
#> [1] NaN
sqrt(-1) # produce:
#> Warning in sqrt(-1): NaNs produced
#> [1] NaN

# Aquí explícitamente falta el price del cuarto trimestre del 2020.
# Y también falta implícitamente toda la columna correspondiente al 
# primer trimestre del 2021
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
# NOTA: Así se definen los valores faltantes:
# Un valor faltante explícito es la presencia de una ausencia.
# Un valor faltante implícito es la ausencia de una presencia.

# Hacer explícitos todos los valores faltantes. Aquí se ve que se 
# volvió explicito el precio faltante del primer trimestre del 2021
stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  ) # produce:
# A tibble: 2 × 5
#   year   `1`   `2`   `3`   `4`
#  <dbl> <dbl> <dbl> <dbl> <dbl>
#1  2020  1.88  0.59  0.35 NA   
#2  2021 NA     0.92  0.17  2.66

# Es posible proporcionar un conjunto de variables que definen la 
# combinación de filas que deberían existir. Para esto se usa la 
# función tidyr::complete()
stocks |>
  complete(year, qtr) # produce:
# A tibble: 8 × 3
#   year   qtr price
#   <dbl> <dbl> <dbl>
#1  2020     1  1.88
#2  2020     2  0.59
#3  2020     3  0.35
#4  2020     4 NA   
#5  2021     1 NA   
#6  2021     2  0.92
#7  2021     3  0.17
#8  2021     4  2.66

# A veces las variables individuales están incompletas, por lo que 
# se pueden proporcionar los propios datos. Por ejemplo, si se sabe 
# que el conjunto de datos stocks abarca el período de 2019 a 2021, 
# se podrían proporcionar explícitamente esos valores para year:
stocks |>
  complete(year = 2019:2021, qtr) # produce:
#> # A tibble: 12 × 3
#>    year   qtr price
#>   <dbl> <dbl> <dbl>
#> 1  2019     1 NA   
#> 2  2019     2 NA   
#> 3  2019     3 NA   
#> 4  2019     4 NA   
#> 5  2020     1  1.88
#> 6  2020     2  0.59
#> # ℹ 6 more rows

library(nycflights13)
#
# Mostrar observaciones implícitamente faltantes al comparar un 
# junto de datos con otro gracias a dplyr::anti_join(x, y). Por 
# ejemplo, se puede usar anti_join para mostrar que falta información
# sobre el destino de 4 aeropuertos:
flights |> 
  distinct(faa = dest) |> 
  anti_join(airports) # OJO: airports no es una variable, es una 
                      # tabla
#
# También se puede ver que falta información sobre el número de
# placa o tailnum de 722 aviones
flights |> 
  distinct(tailnum) |> 
  anti_join(planes) # OJO: planes no es una variable es una tabla
#> Joining with `by = join_by(tailnum)`
#> # A tibble: 722 × 1
#>   tailnum
#>   <chr>  
#> 1 N3ALAA 
#> 2 N3DUAA 
#> 3 N542MQ 
#> 4 N730MQ 
#> 5 N9EAMQ 
#> 6 N532UA 
#> # ℹ 716 more rows
# CONCLUSIÓN: anti_join() recibe de argumento una tabla para
# poder comparar su variable con la misma variable de otra tabla
# que tiene valores faltantes

########################
###
### 18.3.4 Ejercicios
###
########################

# 1. ¿Puedes encontrar alguna relación entre el transportista carriers 
# y las filas que parecen faltar en planes?
#View(planes)
#
# Dejar los vuelos que en la tabla planes no tienen información 
# de tailnum
missing_planes <- flights |> 
  anti_join(planes, by = "tailnum")
missing_planes # produce:
# A tibble: 52,606 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      558            600        -2      753
#2  2013     1     1      559            600        -1      941
#3  2013     1     1      600            600         0      837
#4  2013     1     1      602            605        -3      821
# 
# Contar los carriers de flights
missing_planes |> 
  count(carrier, sort = TRUE) # produce:
# A tibble: 10 × 2
#  carrier     n
#  <chr>   <int>
#1 MQ      25397
#2 AA      22558
#3 UA       1693
#4 9E       1044
# RESPUESTA:
# Las filas faltantes en planes no están distribuidas uniformemente 
# entre carriers: ciertos carriers (particularmente aerolíneas 
# regionales) tienen un porcentaje mucho más alto de aviones sin 
# información en planes.
#
# Sí. Los tailnums faltantes en planes están concentrados en unos 
# pocos carriers. Especialmente algunos regionales como MQ, que 
# aportan una proporción grande de vuelos con aviones sin 
# información registrada.
# Los carriers grandes como WN, F9 o DL casi no tienen tailnums 
# faltantes.