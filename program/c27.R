#####################################
###                               ###
###         Capítulo 27           ###
###        Guía de campo          ###
###                               ###
#####################################

library(tidyverse)

x <- c("one", "two", "three", "four", "five")
# Crear un subconjunto del vector x
x[c(3, 2, 5)] # produce:
#> [1] "three" "two"   "five"

# Crear un conjunto a partir de x
x[c(1, 1, 5, 5, 5, 2)] # produce:
#> [1] "one"  "one"  "five" "five" "five" "two"

# Los valores negativos eliminan los elementos en las posiciones 
# especificadas
x[c(-1, -3, -5)] # produce:
#> [1] "two"  "four"

x <- c(10, 3, NA, 5, 8, 1, NA)
# Seleccionar a todos los valores no faltantes
x[!is.na(x)]
#> [1] 10  3  5  8  1
# Seleccionar a los valores pares y faltantes
x[x %% 2 == 0]
#> [1] 10 NA  8 NA

x <- c(abc = 1, def = 2, xyz = 5)
# Crear un subconjunto de x a partir de argumentos con nombre
x[c("xyz", "def")] # produce:
#> xyz def 
#>   5   2


df <- tibble(
  x = 1:3, 
  y = c("a", "e", "f"), 
  z = runif(3)
)
df # produce:
#A tibble: 3 × 3
#      x y         z
#  <int> <chr> <dbl>
#1     1 a     0.997
#2     2 e     0.119
#3     3 f     0.813
# Seleccionar primera fila y segunda columna
df[1, 2] # produce:
#> # A tibble: 1 × 1
#>   y    
#>   <chr>
#> 1 a
# Seleccionar todas las filas y columnas x e y
df[, c("x" , "y")] # produce:
#> # A tibble: 3 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 e    
#> 3     3 f

# Seleccionar filas donde `x` es mayor a 1 y todas las columnas
df[df$x > 1, ] # produce:
#> # A tibble: 2 × 3
#>       x y         z
#>   <int> <chr> <dbl>
#> 1     2 e     0.834
#> 2     3 f     0.601

df1 <- data.frame(x = 1:3)
df1 # produce:
#  x
#1 1
#2 2
#3 3
df1[, "x"] # produce:
#> [1] 1 2 3
# Nota: Una data frame produce un vector 
#
df2 <- tibble(x = 1:3)
df2 # produce:
# A tibble: 3 × 1
#      x
#  <int>
#1     1
#2     2
#3     3
df2[, "x"] # produce:
#> # A tibble: 3 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     3
# NOTA: Un tibble produce un tibble
# Esta ambiguedad (que un data frame se convierta en un vector) 
# se puede evitar con drop = FALSE:
df1[, "x" , drop = FALSE] # porduce:
#>   x
#> 1 1
#> 2 2
#> 3 3

df <- tibble(
  x = c(2, 3, 1, 1, NA), 
  y = letters[1:5], 
  z = runif(5)
)
df # produce:
# A tibble: 5 × 3
#      x y           z
#  <dbl> <chr>   <dbl>
#1     2 a     0.362  
#2     3 b     0.00596
#3     1 c     0.653  
#4     1 d     0.656 
# Filtrar los mayores a 1
df |> filter(x > 1) # produce:
# A tibble: 2 × 3
#      x y           z
#  <dbl> <chr>   <dbl>
#1     2 a     0.362  
#2     3 b     0.00596
# lo mismo que:
df[!is.na(df$x) & df$x > 1, ] # produce:
# A tibble: 2 × 3
#      x y           z
#  <dbl> <chr>   <dbl>
#1     2 a     0.362  
#2     3 b     0.00596

# Ordenar x e y 
df |> arrange(x, y) # produce:
# A tibble: 5 × 3
#      x y           z
#  <dbl> <chr>   <dbl>
#1     1 c     0.653  
#2     1 d     0.656  
#3     2 a     0.362  
#4     3 b     0.00596
#5    NA e     0.617
# lo mismo que:
df[order(df$x, df$y), ] 

# Seleccionar las columnas "x" y "z"
df |> select(x, z) # produce:
# A tibble: 5 × 2
#      x       z
#  <dbl>   <dbl>
#1     2 0.362  
#2     3 0.00596
#3     1 0.653  
#4     1 0.656  
#5    NA 0.617  
# Lo mismo que:
df[, c("x", "z")]

# Filtrar los valores mayores a 1 en la columna x y de esos seleccionar
# las columnas "y" y "z"
df |> 
  filter(x > 1) |> 
  select(y, z) # produce:
#> # A tibble: 2 × 2
#>   y           z
#>   <chr>   <dbl>
#> 1 a     0.157  
#> 2 b     0.00740
# Esto (usar filter y select al mismo tiempo) es parecido a usar
# la función subset()
# same as
df |> subset(x > 1, c(y, z))

########################
###
### 27.2.4 Ejercicios
###
########################

# NO REALIZADOS

tb <- tibble(
  x = 1:4,
  y = c(10, 4, 1, 21)
)
tb # produce:
# A tibble: 4 × 2
#      x     y
#  <int> <dbl>
#1     1    10
#2     2     4
#3     3     1
#4     4    21
# Seleccionar por posición la primera columna
tb[[1]] # produce:
#> [1] 1 2 3 4
#> Seleccionar por posición la segunda columna
tb[[2]] # produce:
#[1] 10  4  1 21
# Seleccionar la columna "x"
tb[["x"]] # produce:
#> [1] 1 2 3 4
#> Seleccionar la columna "x"
tb$x # produce:
#> [1] 1 2 3 4

# Crear nuevas columnas, similar a mutate:
tb$z <- tb$x + tb$y
tb # produce:
#> # A tibble: 4 × 3
#>       x     y     z
#>   <int> <dbl> <dbl>
#> 1     1    10    11
#> 2     2     4     6
#> 3     3     1     4
#> 4     4    21    25

# Usar $ es conveniente para realizar resúmenes rápidos. Por 
# ejemplo, si solo quiere encontrar el tamaño del diamante más 
# grande o los posibles valores de cut, no es necesario usar 
# summarize():
max(diamonds$carat) # produce:
#> [1] 5.01
# 
# Conocer las categorías de cut:
levels(diamonds$cut) # produce:
#> [1] "Fair"      "Good"      "Very Good" "Premium"   "Ideal"

# pull: toma el nombre de una tabla, una varible y devuelve los
# valores de esa variable:
diamonds |> pull(carat) |> max() # produce:
#> [1] 5.01
diamonds |> pull(carat) # produce:
#[1] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 0.30 0.23 0.22
#[14] 0.31 0.20 0.32 0.30 0.30 0.30 0.30 0.30 0.23 0.23 0.31 0.31 0.23
#[27] 0.24 0.30 0.23 0.23 0.23 0.23 0.23 0.23 0.23 0.23 0.23 0.31 0.26
#[40] 0.33 0.33 0.33 0.26 0.26 0.32 0.29 0.32 0.32 0.25 0.29 0.24 0.23
#
diamonds |> pull(cut) |> levels() # produce:
#> [1] "Fair"      "Good"      "Very Good" "Premium"   "Ideal"
diamonds |> pull(cut) # produce:
#[1] Ideal     Premium   Good      Premium   Good      Very Good
#[7] Very Good Very Good Fair      Very Good Good      Ideal    
#[13] Premium   Ideal     Premium   Premium   Ideal     Good     
#[19] Good      Very Good Good      Very Good Very Good Very Good

df <- data.frame(x1 = 1)
df$x 
#> [1] 1
# NOTA: Aunque la columna x no existe, da el resultado de x1 porque
# r usa lo que se conoce como coincidencia parcial
df$z # produce:
#> NULL
#> NOTA: Si una columna no existe, sus valores no se muestran, obvio
#
# En el caso de las tibles no existe la coincidencia parcial:
tb <- tibble(x1 = 1)
tb$x # produce:
#> Warning: Unknown or uninitialised column: `x`.
#> NULL
tb$z # produce:
#> Warning: Unknown or uninitialised column: `z`.
#> NULL

l <- list(
  a = 1:3, 
  b = "a string", 
  c = pi, 
  d = list(-1, -5)
)

str(l[1:2]) # produce:
#> List of 2
#>  $ a: int [1:3] 1 2 3
#>  $ b: chr "a string"
l[1:2] # produce:
#$a
#[1] 1 2 3
#
#$b
#[1] "a string"
# NOTA: se usa la función str()para que el formato salga más bonito
str(l[1]) # produce:
#> List of 1
#>  $ a: int [1:3] 1 2 3
#
str(l[4]) # produce:
#> List of 1
#>  $ d:List of 2
#>   ..$ : num -1
#>   ..$ : num -5
# NOTA: Si se usan corchetes simples, lo que returna es una lista

# Extraer los elementos de una lista con corchetes dobles
str(l[[1]]) # produce:
#>  int [1:3] 1 2 3
#
str(l[[4]]) # produce:
#> List of 2
#>  $ : num -1
#>  $ : num -5
# NOTA: Aquí hay dos niveles de lista por lo que los corchetes dobles
# devuelven sólo un nivel de la lista
#
str(l$a) # produce:
#>  int [1:3] 1 2 3

########################
###
### 27.3.4 Ejercicios
###
########################

# NO REALIZADOS

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)
df # produce:
# A tibble: 1 × 5
#      a     b c     d         e
#  <dbl> <dbl> <chr> <chr> <dbl>
#1     1     2 a     b         4
# Encontrar las columnas numéricas
num_cols <- sapply(df, is.numeric)
num_cols # produce:
#>     a     b     c     d     e 
#>  TRUE  TRUE FALSE FALSE  TRUE
#
# Y ahora transformar cada columna con lapply() y luego reemplazar los
# valores originales
df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], \(x) x * 2)
df[, num_cols] # produce:
# A tibble: 1 × 3
#      a     b     e
#  <dbl> <dbl> <dbl>
#1     1     2     4
df # produce:
#> # A tibble: 1 × 5
#>       a     b c     d         e
#>   <dbl> <dbl> <chr> <chr> <dbl>
#> 1     2     4 a     b         8

# vapply() es similar a sapply pero acepta un argumento extra que
# especifica el tipo que se espera para sus valores
vapply(df, is.numeric, logical(1)) # produce:
#>     a     b     c     d     e 
#>  TRUE  TRUE FALSE FALSE  TRUE

# tapply() calcula un único resumen agrupado
diamonds |> 
  group_by(cut) |> 
  summarize(price = mean(price)) # produce:
#> # A tibble: 5 × 2
#>   cut       price
#>   <ord>     <dbl>
#> 1 Fair      4359.
#> 2 Good      3929.
#> 3 Very Good 3982.
#> 4 Premium   4584.
#> 5 Ideal     3458.
#
tapply(diamonds$price, diamonds$cut, mean) # produce:
#>      Fair      Good Very Good   Premium     Ideal 
#>  4358.758  3928.864  3981.760  4584.258  3457.542

# Leer todos los archivos de excel en un directorio
paths <- dir("r4ds-progreso/data/gapminder", pattern = "\\.xlsx$", full.names = TRUE)
files <- map(paths, readxl::read_excel)
files # produce:
#[[1]]
#NULL
#
#[[2]]
#NULL
#...

# Crear una lista con la misma longitud que paths
files <- vector("list", length(paths))
files # produce:
#[[1]]
#NULL
#
#[[2]]
#NULL
#...
# Generar un índice para cada elemento de rutas con set_along()
seq_along(paths) # produce:
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12

# Los ínidices son importantes porque nos permiten vincular cada
# posición en la entrada con la posición correspondiente en la salida
for (i in seq_along(paths)) {
  files[[i]] <- readxl::read_excel(paths[[i]])
}
# Para combinar la lista de tibbles en un solo tibble se puede usar 
# do.call()+ rbind():
do.call(rbind, files) # produce:
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

# Otro enfoque es construir el marco de datos en dos piezas:
out <- NULL
for (path in paths) {
  out <- rbind(out, readxl::read_excel(path))
}

# Crear un histograma sin ggplot
hist(diamonds$carat)

# Crear un diagrama de dispersión sin ggplot
plot(diamonds$carat, diamonds$price)