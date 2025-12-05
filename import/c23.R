#####################################
###                               ###
###         Capítulo 23           ###
###      Datos Jerárquicos        ###
###                               ###
#####################################

library(tidyverse)
library(repurrrsive)
library(jsonlite)

# Así se asigna una lista
x1 <- list(1:4, "a", TRUE)
x1  # produce:
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] "a"
#> 
#> [[3]]
#> [1] TRUE

# Nombrar los elementos de una lista
x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2
#> $a
#> [1] 1 2
#> 
#> $b
#> [1] 1 2 3
#> 
#> $c
#> [1] 1 2 3 4

# Para evitar que la vista de retorno  sea tan larga se puede usar
# str()
str(x1) # produce:
#> List of 3
#>  $ : int [1:4] 1 2 3 4
#>  $ : chr "a"
#>  $ : logi TRUE
str(x2) # produce:
#> List of 3
#>  $ a: int [1:2] 1 2
#>  $ b: int [1:3] 1 2 3
#>  $ c: int [1:4] 1 2 3 4

# Las listas pueden contener listas y por eso son tan útiles para
# representar estructuras jeráriquicas (tipo árbol)
x3 <- list(list(1, 2), list(3, 4))
str(x3)
#> List of 2
#>  $ :List of 2
#>   ..$ : num 1
#>   ..$ : num 2
#>  $ :List of 2
#>   ..$ : num 3
#>   ..$ : num 4

# Entonces las listas son notablemente distintas a c() la forma de
# representar vectores por teclado. La c viene de concatenar o
# combinar
c(c(1, 2), c(3, 4)) # produce:
#> [1] 1 2 3 4

x4 <- c(list(1, 2), list(3, 4))
str(x4) # produce:
#> List of 4
#>  $ : num 1
#>  $ : num 2
#>  $ : num 3
#>  $ : num 4

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)
#> List of 2
#>  $ : num 1
#>  $ :List of 2
#>   ..$ : num 2
#>   ..$ :List of 2
#>   .. ..$ : num 3
#>   .. ..$ :List of 2
#>   .. .. ..$ : num 4
#>   .. .. ..$ :List of 1
#>   .. .. .. ..$ : num 5

# Si se complica visualizar una lista,se puede usar View()
#View(x5)

# Ejemplo de columna de lista:
df <- tibble(
  x = 1:2, 
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5)) # Una columna de lista debe
  # representarse como una lista de listas
)
df # preoduce:
#> # A tibble: 2 × 3
#>       x y     z         
#>   <int> <chr> <list>    
#> 1     1 a     <list [2]>
#> 2     2 b     <list [3]>

# Las columnas de listas se comportan exactamente igual a cualquier
# columna:
df |> 
  filter(x == 1) # produce:
#> # A tibble: 1 × 3
#>       x y     z         
#>   <int> <chr> <list>    
#> 1     1 a     <list [2]>


# Si la columna de listas tiene lista con elementos nombrados, estos
# elementos nombrados se desanidan naturalmente en columnas.
df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)
df1 # produce:
# A tibble: 3 × 2
#        x y               
#    <dbl> <list>          
#  1     1 <named list [2]>
#  2     2 <named list [2]>
#  3     3 <named list [2]>

# Las columnas de lista sin nombre se desanidan naturalmente en 
# filas
df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)
df2 # produce:
# A tibble: 3 × 2
#        x y         
#    <dbl> <list>    
#  1     1 <list [3]>
#  2     2 <list [1]>
#  3     3 <list [2]>

# Cuando cada fila tiene la misma cantidad de elementos con los 
# mismos nombres, como df1, es natural colocar cada componente en su 
# propia columna con unnest_wider():
df1 |> 
  unnest_wider(y) # produce:
#> # A tibble: 3 × 3
#>       x     a     b
#>   <dbl> <dbl> <dbl>
#> 1     1    11    12
#> 2     2    21    22
#> 3     3    31    32
# NOTA: desaparece "y" que contiene a "a" y "b"

# Se puede usar el argumento names_sep para combinar el nombre de
# la columna y del elemento. Esto es útil para desambiguar:
df1 |> 
  unnest_wider(y, names_sep = "_") # produce:
#> # A tibble: 3 × 3
#>       x   y_a   y_b
#>   <dbl> <dbl> <dbl>
#> 1     1    11    12
#> 2     2    21    22
#> 3     3    31    32

# Cuando cada fila contiene una lista sin nombre, lo más natural es
# colocar cada elemento en su propia fila con unnest_longer():
df2 |> 
  unnest_longer(y) # produce:
#> # A tibble: 6 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1    11
#> 2     1    12
#> 3     1    13
#> 4     2    21
#> 5     3    31
#> 6     3    32
# NOTA: Esto crea una fila por cada elemento de la lista.

# Sin embargo si una observación tiene una lista vacía, esa fila
# se omite:
df6 <- tribble(
  ~x, ~y,
  "a", list(1, 2),
  "b", list(3),
  "c", list()
)
df6 |> unnest_longer(y) # produce:
#> # A tibble: 3 × 2
#>   x         y
#>   <chr> <dbl>
#> 1 a         1
#> 2 a         2
#> 3 b         3

# Cuando una columna de lista tiene listas con distintos tipos
# unnest_longer() descompone cada elemento de la lista en una fila
# y con su respectivo tipo.
df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)
#
df4 |> 
  unnest_longer(y) # produce:
#> # A tibble: 4 × 2
#>   x     y        
#>   <chr> <list>   
#> 1 a     <dbl [1]>
#> 2 b     <chr [1]>
#> 3 b     <lgl [1]>
#> 4 b     <dbl [1]>
# NOTA: ¿Una columna con listas de elementos de distintos tipos
# contradice la regla de que la columna debe tener valores del
# mismo tipo? NO porque los valores de la columna son todos listas,
# aunque las listas tengan distintos tipos.

########################
###
### 23.3.5 Ejercicios
###
########################

# NO REALIZADOS

