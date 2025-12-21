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



data2019 <- readxl::read_excel("data/y2019.xlsx")
data2020 <- readxl::read_excel("data/y2020.xlsx")
data2021 <- readxl::read_excel("data/y2021.xlsx")
data2022 <- readxl::read_excel("data/y2022.xlsx")

# Leer un directorio lleno de hojas de cálculo:
data2019 <- readxl::read_excel("data/y2019.xlsx")
data2020 <- readxl::read_excel("data/y2020.xlsx")
data2021 <- readxl::read_excel("data/y2021.xlsx")
data2022 <- readxl::read_excel("data/y2022.xlsx")

# Usar dplyr::bind_rows() para juntar todas las tablas
data <- bind_rows(data2019, data2020, data2021, data2022)

getwd() # produce: "/cloud/project"

# Listar todos los archivos de excel con list.files() . El primer
# argumento es el path donde están los archivos, el segundo argumento
# es una expresión regular para filtrar los nombres de los archivos.
# Y full.names determina si el nombre del directorio debe incluirse 
# en la salida.
paths <- list.files("r4ds-progreso/data/gapminder", 
                    pattern = "[.]xlsx$", full.names = TRUE)
paths # produce:
#>  [1] "data/gapminder/1952.xlsx" "data/gapminder/1957.xlsx"
#>  [3] "data/gapminder/1962.xlsx" "data/gapminder/1967.xlsx"
#>  [5] "data/gapminder/1972.xlsx" "data/gapminder/1977.xlsx"
#>  [7] "data/gapminder/1982.xlsx" "data/gapminder/1987.xlsx"
#>  [9] "data/gapminder/1992.xlsx" "data/gapminder/1997.xlsx"
#> [11] "data/gapminder/2002.xlsx" "data/gapminder/2007.xlsx"

# Una forma de incluir a todas las tablas en un sólo objeto es
# ponerlas a todas en una lista
files <- list(
  readxl::read_excel("r4ds-progreso/data/gapminder/1952.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1957.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1962.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1967.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1972.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1977.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1982.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1987.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1992.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/1997.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/2002.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/2007.xlsx"),
  readxl::read_excel("r4ds-progreso/data/gapminder/2007.xlsx")
)
files[[3]] # produce:
#> # A tibble: 142 × 5
#>   country     continent lifeExp      pop gdpPercap
#>   <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 Afghanistan Asia         32.0 10267083      853.
#> 2 Albania     Europe       64.8  1728137     2313.
#> 3 Algeria     Africa       48.3 11000948     2551.
#> 4 Angola      Africa       34    4826015     4269.
#> 5 Argentina   Americas     65.1 21283783     7133.
#> 6 Australia   Oceania      70.9 10794968    12217.
#> # ℹ 136 more rows

# Usar map para poder listar los 12 marcos de datos en una lista
files <- purrr::map(paths, readxl::read_excel)
length(files) # produce:
#> [1] 12
#
files[[1]] # produce:
#> # A tibble: 142 × 5
#>   country     continent lifeExp      pop gdpPercap
#>   <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 Afghanistan Asia         28.8  8425333      779.
#> 2 Albania     Europe       55.2  1282697     1601.
#> 3 Algeria     Africa       43.1  9279525     2449.
#> 4 Angola      Africa       30.0  4232095     3521.
#> 5 Argentina   Americas     62.5 17876956     5911.
#> 6 Australia   Oceania      69.1  8691212    10040.
#> # ℹ 136 more rows

# Usar list_rbind para combinar esa lista de marco de datos en un
# solo marco de datos:
purrr::list_rbind(files) # produce:
#> # A tibble: 1,704 × 5
#>   country     continent lifeExp      pop gdpPercap
#>   <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 Afghanistan Asia         28.8  8425333      779.
#> 2 Albania     Europe       55.2  1282697     1601.
#> 3 Algeria     Africa       43.1  9279525     2449.
#> 4 Angola      Africa       30.0  4232095     3521.
#> 5 Argentina   Americas     62.5 17876956     5911.
#> 6 Australia   Oceania      69.1  8691212    10040.
#> # ℹ 1,698 more rows

# Realizar los dos pasos anteriores a la vez con una tubería:
paths |> 
  purrr::map(readxl::read_excel) |> 
  purrr::list_rbind() # produce:
# A tibble: 1,704 × 5                                                  
#  country     continent lifeExp      pop gdpPercap
#  <chr>       <chr>       <dbl>    <dbl>     <dbl>
#1 Afghanistan Asia         28.8  8425333      779.
#2 Albania     Europe       55.2  1282697     1601.
#3 Algeria     Africa       43.1  9279525     2449.
#4 Angola      Africa       30.0  4232095     3521.

# Echar un vistazo a las primeras filas de los datos con n_max = 1
paths |> 
  purrr::map(\(path) readxl::read_excel(path, n_max = 1)) |> 
  purrr::list_rbind()
#> # A tibble: 12 × 5
#>   country     continent lifeExp      pop gdpPercap
#>   <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 Afghanistan Asia         28.8  8425333      779.
#> 2 Afghanistan Asia         30.3  9240934      821.
#> 3 Afghanistan Asia         32.0 10267083      853.
#> 4 Afghanistan Asia         34.0 11537966      836.
#> 5 Afghanistan Asia         36.1 13079460      740.
#> 6 Afghanistan Asia         38.4 14880372      786.
#> # ℹ 6 more rows
# NOTA: Algo interesante es que no hay la columna año, a pesar de que
# cada archivo se lo llama por su año respectivo

# Asignar un nombre al vector de rutas con set_names() y extraer
# solo el nombre del archivo de la ruta completa:
paths |> purrr::set_names(basename) 
#>                  1952.xlsx                  1957.xlsx 
#> "data/gapminder/1952.xlsx" "data/gapminder/1957.xlsx" 
#>                  1962.xlsx                  1967.xlsx 
#> "data/gapminder/1962.xlsx" "data/gapminder/1967.xlsx" 
#>                  1972.xlsx                  1977.xlsx 
#> "data/gapminder/1972.xlsx" "data/gapminder/1977.xlsx" 
#>                  1982.xlsx                  1987.xlsx 
#> "data/gapminder/1982.xlsx" "data/gapminder/1987.xlsx" 
#>                  1992.xlsx                  1997.xlsx 
#> "data/gapminder/1992.xlsx" "data/gapminder/1997.xlsx" 
#>                  2002.xlsx                  2007.xlsx 
#> "data/gapminder/2002.xlsx" "data/gapminder/2007.xlsx"

#files <- list(
#  "1952.xlsx" = readxl::read_excel("data/gapminder/1952.xlsx"),
#  "1957.xlsx" = readxl::read_excel("data/gapminder/1957.xlsx"),
#  "1962.xlsx" = readxl::read_excel("data/gapminder/1962.xlsx"),
#  ...,
#  "2007.xlsx" = readxl::read_excel("data/gapminder/2007.xlsx")
#) # Este código se puede abreviar así: 
files <- paths |> 
  purrr::set_names(basename) |> 
  purrr::map(readxl::read_excel)
files # produce:
#$`1952.xlsx`
# A tibble: 142 × 5
#  country     continent lifeExp      pop gdpPercap
#  <chr>       <chr>       <dbl>    <dbl>     <dbl>
#1 Afghanistan Asia         28.8  8425333      779.
#2 Albania     Europe       55.2  1282697     1601.
#3 Algeria     Africa       43.1  9279525     2449.
#4 Angola      Africa       30.0  4232095     3521.
# Y así con todos los años.

# Es posible usar corchetes para extraer elementos por nombre:
files[["1962.xlsx"]] # produce:
#> # A tibble: 142 × 5
#>   country     continent lifeExp      pop gdpPercap
#>   <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 Afghanistan Asia         32.0 10267083      853.
#> 2 Albania     Europe       64.8  1728137     2313.
#> 3 Algeria     Africa       48.3 11000948     2551.
#> 4 Angola      Africa       34    4826015     4269.
#> 5 Argentina   Americas     65.1 21283783     7133.
#> 6 Australia   Oceania      70.9 10794968    12217.
#> # ℹ 136 more rows

# Crear una nueva tabla con una nueva columna year
paths |> 
  set_names(basename) |> 
  purrr::map(readxl::read_excel) |> 
  # Guardar los nombres de los archivos en una nueva columna llamada
  # year
  purrr::list_rbind(names_to = "year") |>
  # parse_number(year) extrae el número de la cadena
  mutate(year = readr::parse_number(year)) # produce:
#> # A tibble: 1,704 × 6
#>    year country     continent lifeExp      pop gdpPercap
#>   <dbl> <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1  1952 Afghanistan Asia         28.8  8425333      779.
#> 2  1952 Albania     Europe       55.2  1282697     1601.
#> 3  1952 Algeria     Africa       43.1  9279525     2449.
#> 4  1952 Angola      Africa       30.0  4232095     3521.
#> 5  1952 Argentina   Americas     62.5 17876956     5911.
#> 6  1952 Australia   Oceania      69.1  8691212    10040.
#> # ℹ 1,698 more rows

# También es posible usar set_names() sin argumentos junto con
# separate_wider_delim() para poder usar partes importantes del
# path del archivo de la tabla para incluirlo en la tabla:
paths |> 
  set_names() |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  separate_wider_delim(year, delim = "/", names = c(NA, "dir", "file")) |> 
  separate_wider_delim(file, delim = ".", names = c("file", "ext"))
#> # A tibble: 1,704 × 8
#>   dir       file  ext   country     continent lifeExp      pop gdpPercap
#>   <chr>     <chr> <chr> <chr>       <chr>       <dbl>    <dbl>     <dbl>
#> 1 gapminder 1952  xlsx  Afghanistan Asia         28.8  8425333      779.
#> 2 gapminder 1952  xlsx  Albania     Europe       55.2  1282697     1601.
#> 3 gapminder 1952  xlsx  Algeria     Africa       43.1  9279525     2449.
#> 4 gapminder 1952  xlsx  Angola      Africa       30.0  4232095     3521.
#> 5 gapminder 1952  xlsx  Argentina   Americas     62.5 17876956     5911.
#> 6 gapminder 1952  xlsx  Australia   Oceania      69.1  8691212    10040.
#> # ℹ 1,698 more rows


gapminder <- paths |> 
  set_names(basename) |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  mutate(year = parse_number(year))
# Guardar marco de datos
write_csv(gapminder, "gapminder.csv")
# NOTA: el archivo gapminder.csv se guardará en el directorio
# actual de trabajo

# Cargar todos los archivos
files <- paths |> 
  map(readxl::read_excel) 
files # produce:
# A tibble: 142 × 5
#  country     continent lifeExp       pop gdpPercap
#  <chr>       <chr>       <dbl>     <dbl>     <dbl>
#1 Afghanistan Asia         43.8  31889923      975.
#2 Albania     Europe       76.4   3600523     5937.
#3 Algeria     Africa       72.3  33333216     6223.
#4 Angola      Africa       42.7  12420476     4797.

