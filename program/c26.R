#####################################
###                               ###
###         Capítulo 26           ###
###          Iteración            ###
###                               ###
#####################################

library(tidyverse)

# Un tibble simple
set.seed(1014)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df # produce:
# A tibble: 10 × 4
#   a        b       c      d
#   <dbl>    <dbl>   <dbl>  <dbl>
#1  2.21  -0.870   -0.332   0.440
#2 -1.20   1.18    -0.661   1.37 
#3  0.569 -0.461   -0.304   0.776
#4 -0.900 -2.76     0.0221  1.02

# Calcular la mediana de cada columna:
df |> summarize(
  n = n(),
  a = median(a),
  b = median(b),
  c = median(c),
  d = median(d),
) # produce:
# A tibble: 1 × 5
#        n     a      b      c     d
#    <int> <dbl>  <dbl>  <dbl> <dbl>
#  1    10 0.556 -0.329 -0.296 0.830
# NOTA: Esto rompe la regla de de nunca copiar y pegar más de dos 
# veces. En lugar de esto se puede usar across:
df |> summarize(
  n = n(),
  across(a:d, median),
) # produce:
#> # A tibble: 1 × 5
#>       n      a      b       c     d
#>   <int>  <dbl>  <dbl>   <dbl> <dbl>
#> 1    10 -0.246 -0.287 -0.0567 0.144

# across es una función de orden superior que podría recibir funciones
# importantes como everything()y where()
set.seed(1014)
df <- tibble(
  grp = sample(2, 10, replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
# everything() selecciona todas las columnas no agrupadas:
df |> 
  group_by(grp) |> 
  summarize(across(everything(), median))
#> # A tibble: 2 × 5
#>     grp      a      b       c       d
#>   <int>  <dbl>  <dbl>   <dbl>   <dbl>
#> 1     1 -0.244 -0.522 -0.0974 -0.251 
#> 2     2 -0.247  0.468  0.112   0.0700
# NOTA: median es una función pero se llama sin paréntesis porque
# es una función que recibe variables y median() es una llamada a
# función sin variables.
df |> 
  group_by(grp) |> 
  summarize(across(everything(), median())) 
# produce: error
#
median() # produce: 
# Error in median.default(): argument "x" is missing, with no 
# default

set.seed(1014)
rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  sample(c(rnorm(n - n_na, mean = mean, sd = sd), rep(NA, n_na)))
}
#
df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)
df_miss # produce:
# A tibble: 5 × 4
#         a      b      c        d
#     <dbl>  <dbl>  <dbl>    <dbl>
#1 -0.00557 -0.283 -1.86  -0.783  
#2  0.255   -0.247 -0.522 -0.00289
#3 -1.40    -0.554  0.512  0.413  
#4 -2.44    -0.244 NA      0.724  
#5 NA       NA     NA      2.35
df_miss |> 
  summarize(
    across(a:d, median),
    n = n()
  ) # produce:
#> # A tibble: 1 × 5
#>       a     b     c     d     n
#>   <dbl> <dbl> <dbl> <dbl> <int>
#> 1    NA    NA    NA 0.413     5
# Este resultado es subóptimo porque propaga los valores faltantes
# en la tibble

# Para evitar que los valores faltantes den resultados subóptimos
# se puede usar la función across con la función median() pero 
# tomando en cuenta los valores faltantes
df_miss |> 
  summarize(
    across(a:d, function(x) median(x, na.rm = TRUE)), # esta es una
    # función anónima, que se puede cambiar por un slash ""\ , así:
    #across(a:d, \(x) median(x, na.rm = TRUE)),
    n = n()
  ) # produce:
#> # A tibble: 1 × 5
#>        a      b      c     d     n
#>    <dbl>  <dbl>  <dbl> <dbl> <int>
#> 1 -0.703 -0.265 -0.522 0.413     5
#
# across se expande efectivamente al siguiente código:
df_miss |> 
  summarize(
    a = median(a, na.rm = TRUE),
    b = median(b, na.rm = TRUE),
    c = median(c, na.rm = TRUE),
    d = median(d, na.rm = TRUE),
    n = n()
  )

# Calcular la mediana y contar los valores faltantes:
df_miss |> 
  summarize(
    across(a:d, list(
      median = \(x) median(x, na.rm = TRUE),
      n_miss = \(x) sum(is.na(x)) # contar los valores faltantes
    )),
    n = n()
  ) # produce:
#> # A tibble: 1 × 9
#>   a_median a_n_miss b_median b_n_miss c_median c_n_miss d_median d_n_miss
#>      <dbl>    <int>    <dbl>    <int>    <dbl>    <int>    <dbl>    <int>
#> 1   -0.703        1   -0.265        1   -0.522        2    0.413        0
#> # ℹ 1 more variable: n <int>

# Expandir todas las columnas de fecha a columnas de año, mes y día
expand_dates <- function(df) {
  df |> 
    mutate(
      across(where(is.Date), list(year = year, month = month, day = mday))
    )
}
#
df_date <- tibble(
  name = c("Amy", "Bob"),
  date = ymd(c("2009-08-03", "2010-01-16"))
)
#
df_date |> 
  expand_dates() # produce:
#> # A tibble: 2 × 5
#>   name  date       date_year date_month date_day
#>   <chr> <date>         <dbl>      <dbl>    <int>
#> 1 Amy   2009-08-03      2009          8        3
#> 2 Bob   2010-01-16      2010          1       16

# Calcular la media de las columnas numéricas por defecto:
summarize_means <- function(df, summary_vars = where(is.numeric)) {
  df |> 
    summarize(
      across({{ summary_vars }}, \(x) mean(x, na.rm = TRUE)),
      n = n(),
      .groups = "drop"
    )
}
diamonds |> 
  group_by(cut) |> 
  summarize_means() # produce:
#> # A tibble: 5 × 9
#>   cut       carat depth table price     x     y     z     n
#>   <ord>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <int>
#> 1 Fair      1.05   64.0  59.1 4359.  6.25  6.18  3.98  1610
#> 2 Good      0.849  62.4  58.7 3929.  5.84  5.85  3.64  4906
#> 3 Very Good 0.806  61.8  58.0 3982.  5.74  5.77  3.56 12082
#> 4 Premium   0.892  61.3  58.7 4584.  5.97  5.94  3.65 13791
#> 5 Ideal     0.703  61.7  56.0 3458.  5.51  5.52  3.40 21551
#
diamonds |> 
  group_by(cut) |> 
  summarize_means(c(carat, x:z))
#> # A tibble: 5 × 6
#>   cut       carat     x     y     z     n
#>   <ord>     <dbl> <dbl> <dbl> <dbl> <int>
#> 1 Fair      1.05   6.25  6.18  3.98  1610
#> 2 Good      0.849  5.84  5.85  3.64  4906
#> 3 Very Good 0.806  5.74  5.77  3.56 12082
#> 4 Premium   0.892  5.97  5.94  3.65 13791
#> 5 Ideal     0.703  5.51  5.52  3.40 21551

# Resumen multifunción:
df |> 
  summarize(across(a:d, list(median = median, mean = mean)))
# produce:
#> # A tibble: 1 × 8
#>   a_median  a_mean b_median  b_mean c_median  c_mean d_median d_mean
#>      <dbl>   <dbl>    <dbl>   <dbl>    <dbl>   <dbl>    <dbl>  <dbl>
#> 1   -0.246 -0.0426    0.155 -0.0656   0.0480 -0.0297   -0.193 -0.200

# Calcular los mismos resúmenes pero pivotando por más tiempo y luego
# resumiendo:
long <- df |> 
  pivot_longer(a:d) |> 
  group_by(name) |> 
  summarize(
    median = median(value),
    mean = mean(value)
  )
long
#> # A tibble: 4 × 3
#>   name   median    mean
#>   <chr>   <dbl>   <dbl>
#> 1 a     -0.246  -0.0426
#> 2 b      0.155  -0.0656
#> 3 c      0.0480 -0.0297
#> 4 d     -0.193  -0.200

#  Si se quiere la misma estructura across se puede pivotar 
# nuevamente: 
long |> 
  pivot_wider(
    names_from = name,
    values_from = c(median, mean),
    names_vary = "slowest",
    names_glue = "{name}_{.value}"
  )
#> # A tibble: 1 × 8
#>   a_median  a_mean b_median  b_mean c_median  c_mean d_median d_mean
#>      <dbl>   <dbl>    <dbl>   <dbl>    <dbl>   <dbl>    <dbl>  <dbl>
#> 1   -0.246 -0.0426    0.155 -0.0656   0.0480 -0.0297   -0.193 -0.200

########################
###
### 26.2.8 Ejercicios
###
########################

# NO REALIZADOS

