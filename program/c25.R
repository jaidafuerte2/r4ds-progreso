#####################################
###                               ###
###         Capítulo 25           ###
###          Funciones            ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
)
df # produce:
# A tibble: 5 × 4
#      a      b      c       d
#  <dbl>  <dbl>  <dbl>   <dbl>
#1 0.695 -0.766  0.149 -0.0681
#2 1.45  -0.134  0.462  0.112 
#3 1.35   0.515  0.503  0.559 
#4 0.974  0.879 -0.304  0.303 
#5 0.350  1.27   0.694  0.278 
df |> mutate(
  a = (a - min(a, na.rm = TRUE)) / 
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),
  b = (b - min(a, na.rm = TRUE)) / # Aquí hay un error porque se
                                   # está usando a en vez de b
    (max(b, na.rm = TRUE) - min(b, na.rm = TRUE)),
  c = (c - min(c, na.rm = TRUE)) / 
    (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),
  d = (d - min(d, na.rm = TRUE)) / 
    (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)),
) # produce:
#> # A tibble: 5 × 4
#>       a       b     c     d
#>   <dbl>   <dbl> <dbl> <dbl>
#> 1 0.339  0.387  0.291 0    
#> 2 0.880 -0.613  0.611 0.557
#> 3 0     -0.0833 1     0.752
#> 4 0.795 -0.0822 0     1    
#> 5 1     -0.0952 0.580 0.394

# Se puede rescribir el código anterior con funciones para evitar
# errores como el anterior:
rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - 
                                  min(x, na.rm = TRUE))
}
rescale01(c(-10, 0, 10)) # produce:
#> [1] 0.0 0.5 1.0
rescale01(c(1, 2, 3, NA, 5)) # produce:
#> [1] 0.00 0.25 0.50   NA 1.00
#
# Entonces rescribimos la llamada a mutate()
df |> mutate(
  a = rescale01(a),
  b = rescale01(b),
  c = rescale01(c),
  d = rescale01(d),
) # produce:
#> # A tibble: 5 × 4
#>       a     b     c     d
#>   <dbl> <dbl> <dbl> <dbl>
#> 1 0.339 1     0.291 0    
#> 2 0.880 0     0.611 0.557
#> 3 0     0.530 1     0.752
#> 4 0.795 0.531 0     1    
#> 5 1     0.518 0.580 0.394

rng <- range(c(0, 3, 5), na.rm = TRUE)
rng # produce: [1] 0 5
rng <- range(c(0, 5, 3), na.rm = TRUE)
rng # produce: [1] 0 5
# range devuelve el número mínimo y máximo de un vector
#
# Entonces se puede rescribir rescale01 con range, sin necesidad
# de usar min() y max()
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
x <- c(1:10, Inf)
rescale01(x) # produce:
#>  [1]   0   0   0   0   0   0   0   0   0   0 NaN
# NOTA: Este no es el resultado esperado por lo que se puede pedir a 
# range que ignore los valores infinitos
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x) # produce:
#>  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
#>  [8] 0.7777778 0.8888889 1.0000000       Inf

# Garantizar que todos los valores de un vector se encuentren entre
# un mínimo y un máximo.
clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}
clamp(1:10, min = 3, max = 7) # produce:
#>  [1] 3 3 3 4 5 6 7 7 7 7

x <- "hola"
# Devuelve los caracteres desde 1 hasta 1 con este nuevo valor.
str_sub(x, 1, 1) # produce: [1] "h"
str_to_upper("h") # produce: "H"
str_sub(x, 1, 1) <- "H"
x # produce: Hola
# Entonces, capitalizar la primera letra de una palabra es 
# relativamente simple:
first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}
#
first_upper("hello")
#> [1] "Hello"

# Calcular la puntuación Z, reescalando un vector para que tenga una 
# media de cero y una desviación estándar de uno:
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Mostrar los valores de un vector garantizando que se encuentren 
# entre un mínimo y un máximo
clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}
clamp(1:10, min = 3, max = 7) # produce:
#>  [1] 3 3 3 4 5 6 7 7 7 7

# Convertir el primer caracter en mayúscula
first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}
first_upper("hello")
#> [1] "Hello"

# eliminar los signos de porcentaje, comas y signos de dólar de una 
# cadena antes de convertirla en un número:
clean_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x |> 
    str_remove_all("%") |> 
    str_remove_all(",") |> 
    str_remove_all(fixed("$")) |> 
    as.numeric()
  if_else(is_pct, num / 100, num)
}
clean_number("$12,300") # produce:
#> [1] 12300
clean_number("45%") # produce:
#> [1] 0.45

# Reemplazar valores faltantes que se registran como 997,998 o 999
fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}
fix_na(c(997, 998, 999, 1000)) # produce: [1]   NA   NA   NA 1000

# Devolver una sóla cadena con algunos caracteres intermedios
commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}
commas(c("cat", "dog", "bird", "pigeon")) # produce:
#> [1] "cat, dog, bird and pigeon"

# Calcular el coeficiente de variación, que divide la desviación
# estándar por la media
cv <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
cv(runif(100, min = 0, max = 50)) # produce:
#> [1] 0.5196276
cv(runif(100, min = 0, max = 500)) # produce:
#> [1] 0.5652554

# Renombrar una función para que sea más fácil de recordar:
n_missing <- function(x) {
  sum(x)
} 
n_missing(c(1, 4, 7, 9)) # produce:
#[1] 21 

# calcular el error porcentual absoluto medio para comparar las 
# predicciones del modelo con los valores reales:
mape <- function(actual, predicted) {
  sum(abs((actual - predicted) / actual)) / length(actual)
}

########################
###
### 25.2.5 Ejercicios
###
########################

# 1. Practica convertir los siguientes fragmentos de código en funciones. Piensa en la función de cada función. ¿Cómo la llamarías? ¿Cuántos argumentos necesita?

#mean(is.na(x))
#mean(is.na(y))
#mean(is.na(z))
# Calcular la proporción de valores NA en un vector
prop_na <- function(x) {
  mean(is.na(x))
}
prop_na(c(1, 2, 3, NA)) # produce: 0.25

#x / sum(x, na.rm = TRUE)
#y / sum(y, na.rm = TRUE)
#z / sum(z, na.rm = TRUE)
# Convertir un vector numérico en proporciones que suman 1 (ignorando
# NA)
prop_sum <- function(x) {
  x / sum(x, na.rm = TRUE)
}
prop_sum(c(1,2,3,4,5)) # produce:
# [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333

#round(x / sum(x, na.rm = TRUE) * 100, 1)
#round(y / sum(y, na.rm = TRUE) * 100, 1)
#round(z / sum(z, na.rm = TRUE) * 100, 1) 
# Convertir un vector en porcentajes y redondearlo
percent <- function(x, digits = 1) {
  round(x / sum(x, na.rm = TRUE) * 100, digits)
}
percent(c(1,2,3,4,5)) # produce:
# [1]  6.7 13.3 20.0 26.7 33.3

# 2. : NO RESUELTO

# 3. Dado un vector de fechas de nacimiento, escriba una función 
# para calcular la edad en años.
Sys.Date() # produce: [1] "2025-12-13"
age_years <- function(birth_date, ref_date = Sys.Date()) {
  # Se asigna fechas a variables 
  birth_date <- as.Date(birth_date)
  ref_date <- as.Date(ref_date)
  # Aquí se calcula la edad 
  age <- as.integer(format(ref_date, "%Y")) -
    as.integer(format(birth_date, "%Y"))
  # Se calcula si el cumpleaños ya pasó en este año
  had_birthday <- format(ref_date, "%m-%d") >=
    format(birth_date, "%m-%d")
  # Si el cumpleaños aún no pasa en este año, se resta uno a la edad
  age - ifelse(had_birthday, 0, 1)
}
birth <- as.Date(c("1985-06-10", "2000-12-01", "1995-01-20"))
age_years(birth) # produce: [1] 40 25 30

# 4. Escribe tus propias funciones para calcular la varianza y la 
# asimetría de un vector numérico.
# Función para la varianza
my_var <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  
  n <- length(x)
  mu <- mean(x)
  
  sum((x - mu)^2) / (n - 1)
}
all.equal(my_var(c(1,2,3,4,5)), var(c(1,2,3,4,5)))

# Función para calcular la asimetría
skewness <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  
  mu <- mean(x)
  s <- sd(x)
  n <- length(x)
  
  sum(((x - mu) / s)^3) / n
}
skewness(c(-2, -1, 0, 1, 2)) # produce: [1] 0

# 5. Escriba both_na(), una función de resumen que toma dos vectores 
# de la misma longitud y devuelve el número de posiciones que 
# tienen un NA en ambos vectores.
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}
x <- c(1, NA, 3, NA)
y <- c(NA, NA, 3, NA)
both_na(x, y) # produce: [1] 2

# 6. Lee la documentación para comprender la función de las siguientes 
# funciones. ¿Por qué son útiles a pesar de su brevedad?
is_directory <- function(x) {
  file.info(x)$isdir
}
is_readable <- function(x) {
  file.access(x, 4) == 0 # cero si se puede leer
}
?file.info()
#isdir	->
#logical: Is the file a directory?
?file.access()
#4
#test for read permission.
# file.access(x, 4) == 0 #  Devuelve TRUE si el archivo existe y
# se puede leer, de otra forma FALSE.
# Mi parecer es que ese renombra a las funciones para que sea un
# nombre más intuitivo y tal vez no complicarse tanto leyendo el código.
# Se pierde en transparencia  pero se gana en legibilidad.

# Calcular la media de mean_var agrupado por group_var:
grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by(group_var) |> 
    summarize(mean(mean_var))
}
diamonds |> grouped_mean(cut, carat) # produce:
#> Error in `group_by()`:
#> ! Must group by variables found in `.data`.
#> ✖ Column `group_var` is not found.

df <- tibble(
  mean_var = 1,
  group_var = "g",
  group = 1,
  x = 10,
  y = 100
)
df |> grouped_mean(group, x) # produce:
#> # A tibble: 1 × 2
#>   group_var `mean(mean_var)`
#>   <chr>                <dbl>
#> 1 g                        1
# NOTA: El problema parece que es que la función usa los mismos
# nombres para sus argumentos que los nombres de las variables
# de la tabla. Este es un problema de indirección
df |> grouped_mean(group, y) # produce:
#> # A tibble: 1 × 2
#>   group_var `mean(mean_var)`
#>   <chr>                <dbl>
#> 1 g                        1

# Para solucionar el problema de indirección debemos abrazar las
# variables entre llaves
grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by({{ group_var }}) |> 
    summarize(mean({{ mean_var }}))
}
df |> grouped_mean(group, x) # produce:
#> # A tibble: 1 × 2
#>   group `mean(x)`
#>   <dbl>     <dbl>
#> 1     1        10

# Encapsular un conjunto de resúmenes al realizar la exploración inicial 
# de datos:
summary6 <- function(data, var) {
  data |> summarize(
    min = min({{ var }}, na.rm = TRUE),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    max = max({{ var }}, na.rm = TRUE),
    n = n(),
    n_miss = sum(is.na({{ var }})),
    .groups = "drop"
  )
}
diamonds |> summary6(carat) # produce:
#> # A tibble: 1 × 6
#>     min  mean median   max     n n_miss
#>   <dbl> <dbl>  <dbl> <dbl> <int>  <int>
#> 1   0.2 0.798    0.7  5.01 53940      0
# NOTA: Siempre que se usa summarize es una buena práctica configurar
# .groups = "drop" para evitar el mensaje y dejar los datos en un 
# estado no agrupado

# Se puede usar la función summary6 en datos agrupados porque la 
# función summary6() encapsula a la función summary()
diamonds |> 
  group_by(cut) |> 
  summary6(carat) # produce:
#> # A tibble: 5 × 7
#>   cut         min  mean median   max     n n_miss
#>   <ord>     <dbl> <dbl>  <dbl> <dbl> <int>  <int>
#> 1 Fair       0.22 1.05    1     5.01  1610      0
#> 2 Good       0.23 0.849   0.82  3.01  4906      0
#> 3 Very Good  0.2  0.806   0.71  4    12082      0
#> 4 Premium    0.2  0.892   0.86  4.01 13791      0
#> 5 Ideal      0.2  0.703   0.54  3.5  21551      0

# Tmabién se podrían resumir las variables que se pasen como 
# argumento a summary6():
diamonds |> 
  group_by(cut) |> 
  summary6(log10(carat)) # produce:
#> # A tibble: 5 × 7
#>   cut          min    mean  median   max     n n_miss
#>   <ord>      <dbl>   <dbl>   <dbl> <dbl> <int>  <int>
#> 1 Fair      -0.658 -0.0273  0      0.700  1610      0
#> 2 Good      -0.638 -0.133  -0.0862 0.479  4906      0
#> 3 Very Good -0.699 -0.164  -0.149  0.602 12082      0
#> 4 Premium   -0.699 -0.125  -0.0655 0.603 13791      0
#> 5 Ideal     -0.699 -0.225  -0.268  0.544 21551      0

# También se puede usar count() en funciones para calcular 
# proporciones:
# https://twitter.com/Diabb6/status/1571635146658402309
count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n))
}
diamonds |> count_prop(clarity) # produce:
#> # A tibble: 8 × 3
#>   clarity     n   prop
#>   <ord>   <int>  <dbl>
#> 1 I1        741 0.0137
#> 2 SI2      9194 0.170 
#> 3 SI1     13065 0.242 
#> 4 VS2     12258 0.227 
#> 5 VS1      8171 0.151 
#> 6 VVS2     5066 0.0939
#> # ℹ 2 more rows

# Permitir que el usuario proporcione una condición:
unique_where <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    distinct({{ var }}) |> 
    arrange({{ var }})
}
# Encontrar todos los destinos en Diciembre:
flights |> unique_where(month == 12, dest)
#> # A tibble: 96 × 1
#>   dest 
#>   <chr>
#> 1 ABQ  
#> 2 ALB  
#> 3 ATL  
#> 4 AUS  
#> 5 AVL  
#> 6 BDL  
#> # ℹ 90 more rows

# Seleccionar ciertas columnas de manera predeterminada y otras
# que se pasen como argumentos, dentro de una tabla "rows":
subset_flights <- function(rows, cols) {
  flights |> 
    filter({{ rows }}) |> 
    select(time_hour, carrier, flight, {{ cols }})
}

# Contar el número de observaciones faltantes en las filas:
count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by({{ group_vars }}) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}
flights |> 
  count_missing(c(year, month, day), dep_time)
#> Error in `group_by()`:
#> ℹ In argument: `c(year, month, day)`.
#> Caused by error:
#> ! `c(year, month, day)` must be size 336776 or 1, not 1010328.
# NOTA: Esto no funciona porque group_by()usa enmascaramiento de 
# datos, no selección ordenada. 
# 
# Para solucionar este problema se puede usar la función pick(), que
# usa la selección ordenada dentro de funciones de enmascaraminto de
# datos
count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}
flights |> 
  count_missing(c(year, month, day), dep_time) # produce:
#> # A tibble: 365 × 4
#>    year month   day n_miss
#>   <int> <int> <int>  <int>
#> 1  2013     1     1      4
#> 2  2013     1     2      8
#> 3  2013     1     3     10
#> 4  2013     1     4      6
#> 5  2013     1     5      3
#> 6  2013     1     6      1
#> # ℹ 359 more rows

# También se puede usar pick() para crear una tabla de conteo en 2D
# Aquí contamos usando todas las variables en rows y columns, y luego 
# usamos pivot_wider()para reorganizar los conteos en una 
# cuadrícula:
count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}

diamonds |> count_wide(c(clarity, color), cut)
#> # A tibble: 56 × 7
#>   clarity color  Fair  Good `Very Good` Premium Ideal
#>   <ord>   <ord> <int> <int>       <int>   <int> <int>
#> 1 I1      D         4     8           5      12    13
#> 2 I1      E         9    23          22      30    18
#> 3 I1      F        35    19          13      34    42
#> 4 I1      G        53    19          16      46    16
#> 5 I1      H        52    14          12      46    38
#> 6 I1      I        34     9           8      24    17
#> # ℹ 50 more rows

diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.1)
#
diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.05)
#
# Resumir estas dos últimas expresiones en una función de histograma
# gracias a que aes es una función de enmascaramiento de datos:
histogram <- function(df, var, binwidth = NULL) {
  df |> 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth)
}
diamonds |> histogram(carat, 0.1)

# NOTA: La función histogram()devuelve un gráfico ggplot2, lo que
# significa que puede agregar componentes adicionales si lo desea. 
# Solo recuerde cambiar de |> a +:
diamonds |> 
  histogram(carat, 0.1) +
  labs(x = "Size (in carats)", y = "Number of diamonds")

# determinar si un conjunto de datos es lineal superponiendo una
# línea suave y una recta:
# https://twitter.com/tyler_js_smith/status/1574377116988104704
linearity_check <- function(df, x, y) {
  df |>
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "loess", formula = y ~ x, color = "red", se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) 
}
#
starwars |> 
  filter(mass < 1000) |> 
  linearity_check(mass, height) 

# Una alternativa a un diagrama de dispersión para conjuntos de datos
# demasiado grandes donde la sobrexposición es un problema, puede ser 
# la función stat_summary_hex():
hex_plot <- function(df, x, y, z, bins = 20, fun = "mean") {
  df |> 
    ggplot(aes(x = {{ x }}, y = {{ y }}, z = {{ z }})) + 
    stat_summary_hex(
      aes(color = after_scale(fill)), # make border same color as fill
      bins = bins, 
      fun = fun,
    )
}
#
diamonds |> hex_plot(carat, price, depth)

# Crear un gráfico de barras verticales donde las barras se ordenan 
# automáticamente por frecuencia usando fct_infreq(). Dado que el 
# gráfico es vertical por defecto también necesitamos invertir el
# orden por defecto para obtener los valores más altos en la parte 
# superior.
sorted_bars <- function(df, var) {
  df |> 
    # Es posible que invertir el orden por defecto sea función de
    # fct_rev()
    mutate({{ var }} := fct_rev(fct_infreq({{ var }})))  |>
    ggplot(aes(y = {{ var }})) +
    geom_bar()
}
diamonds |> sorted_bars(clarity)
# NOTA: ":=" es conocido como operador morsa o "asignador con nombre
# programático". Se usa en vez de <- e  = cuando una función recibe
# un argumento y a ese argumento {{var}} se le quiere asignar un valor
# en la función. Se usa porque <- e = no funciona en estos casos

# Elaborar un gráfico de barras sólo para un subconjunto de los
# datos
conditional_bars <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    ggplot(aes(x = {{ var }})) + 
    geom_bar()
}
#
diamonds |> conditional_bars(cut == "Good", clarity)

# Etiquetar la salida con la variable y el ancho del bin utilizado,
# usando la función rlang::englue() . Por lo que cualquier valor
# incluido entre {} se insertará en la cadena:
histogram <- function(df, var, binwidth) {
  label <- rlang::englue("A histogram of {{var}} with binwidth {binwidth}")
  
  df |> 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth) + 
    labs(title = label)
}
#
diamonds |> histogram(carat, 0.1)

########################
###
### 25.4.4 Ejercicios
###
########################

# NO REALIZADOS

# NOTA DE ESTILO IMPORTANTE: El nombre de una función debe ser corto
# pero que refleje claramente su función; sin embargo es mejor ser 
# claro que corto. Además los nombres de las funciones deben ser
# verbos y los argumentos sustantivos.
#
# Too short
f()
#
# Not a verb, or descriptive
my_awesome_function()
#
# Long, but clear
impute_missing()
collapse_years()

# NOTA DE ESTILO: Las funciones deben ir con un espacio y llaves
# después de function. Y después de abrir las llaves se debe sangrar
# el código. Todo el código debe sangrarse depsués de las llaves, 
# según las reglas:
#
# Missing extra two spaces
density <- function(color, facets, binwidth = 0.1) {
diamonds |> 
  ggplot(aes(x = carat, y = after_stat(density), color = {{ color }})) +
  geom_freqpoly(binwidth = binwidth) +
  facet_wrap(vars({{ facets }}))
}
#
# Pipe indented incorrectly
density <- function(color, facets, binwidth = 0.1) {
  diamonds |> 
  ggplot(aes(x = carat, y = after_stat(density), color = {{ color }})) +
  geom_freqpoly(binwidth = binwidth) +
  facet_wrap(vars({{ facets }}))
}

########################
###
### 25.5.1 Ejercicios
###
########################

# NO REALIZADOS
