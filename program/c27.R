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

