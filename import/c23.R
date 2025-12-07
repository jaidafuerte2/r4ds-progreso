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

# gh_repos es una lista muy anidada que contiene datos sobre una 
# colección de repositorios de github obtenidos mediante la api de github
View(gh_repos)

# gh_repos es una lista de lista, pero necesitamos trabajar en r con
# una columna de listas así que ponemos gh_repos en un tibble
repos <- tibble(json = gh_repos)
repos # produce:
#> # A tibble: 6 × 1
#>   json       
#>   <list>     
#> 1 <list [30]>
#> 2 <list [30]>
#> 3 <list [30]>
#> 4 <list [26]>
#> 5 <list [30]>
#> 6 <list [30]>

# Cada una de las 6 filas contiene una lista SIN nombre con 26 o 30
# filas, por lo que se usará unnest_longer()
repos |> 
  unnest_longer(json) # produce:
#> # A tibble: 176 × 1
#>   json             
#>   <list>           
#> 1 <named list [68]>
#> 2 <named list [68]>
#> 3 <named list [68]>
#> 4 <named list [68]>
#> 5 <named list [68]>
#> 6 <named list [68]>
#> # ℹ 170 more rows
# NOTA: Ahora cada elemento es una lista con nombre, es decir que
# ahora ya es posible usar unnest_wider() para colocar cada elemento
# en su propia columna
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) #  produce:
#> # A tibble: 176 × 68
#>         id name        full_name         owner        private html_url       
#>      <int> <chr>       <chr>             <list>       <lgl>   <chr>          
#> 1 61160198 after       gaborcsardi/after <named list> FALSE   https://github…
#> 2 40500181 argufy      gaborcsardi/argu… <named list> FALSE   https://github…
#> 3 36442442 ask         gaborcsardi/ask   <named list> FALSE   https://github…
#> 4 34924886 baseimports gaborcsardi/base… <named list> FALSE   https://github…
#> 5 61620661 citest      gaborcsardi/cite… <named list> FALSE   https://github…
#> 6 33907457 clisymbols  gaborcsardi/clis… <named list> FALSE   https://github…
#> # ℹ 170 more rows
#> # ℹ 62 more variables: description <chr>, fork <lgl>, url <chr>, …
#> 
#> # Se puede ver el nombre de las el nombre de las columnas con la 
#> # función names()
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  names() |> 
  head(10) # Esta función se utiliza para ver los 10 primeros nombres
  # de las columnas
#
# Seleccionar algunos nombres de variables o columnas
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) # produce:
#> # A tibble: 176 × 4
#>         id full_name               owner             description             
#>      <int> <chr>                   <list>            <chr>                   
#> 1 61160198 gaborcsardi/after       <named list [17]> Run Code in the Backgro…
#> 2 40500181 gaborcsardi/argufy      <named list [17]> Declarative function ar…
#> 3 36442442 gaborcsardi/ask         <named list [17]> Friendly CLI interactio…
#> 4 34924886 gaborcsardi/baseimports <named list [17]> Do we get warnings for …
#> 5 61620661 gaborcsardi/citest      <named list [17]> Test R package and repo…
#> 6 33907457 gaborcsardi/clisymbols  <named list [17]> Unicode symbols for CLI…
#> # ℹ 170 more rows

# owner es otra columna de lista que contiene listas nombradas, cuyos 
# elementos se pueden acceder con unnest_wider():
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner)
#> Error in `unnest_wider()`:
#> ! Can't duplicate names between the affected columns and the original
#>   data.
#> ✖ These names are duplicated:
#>   ℹ `id`, from `owner`.
#> ℹ Use `names_sep` to disambiguate using the column name.
#> ℹ Or use `names_repair` to specify a repair strategy.
#
# Esta columna de lista tiene un id y no se puede tener 2 columnas
# id en la misma tablas. para resolver esto es posible usar names_sep()
# para desambiguar
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_")
#> # A tibble: 176 × 20
#>         id full_name               owner_login owner_id owner_avatar_url     
#>      <int> <chr>                   <chr>          <int> <chr>                
#> 1 61160198 gaborcsardi/after       gaborcsardi   660288 https://avatars.gith…
#> 2 40500181 gaborcsardi/argufy      gaborcsardi   660288 https://avatars.gith…
#> 3 36442442 gaborcsardi/ask         gaborcsardi   660288 https://avatars.gith…
#> 4 34924886 gaborcsardi/baseimports gaborcsardi   660288 https://avatars.gith…
#> 5 61620661 gaborcsardi/citest      gaborcsardi   660288 https://avatars.gith…
#> 6 33907457 gaborcsardi/clisymbols  gaborcsardi   660288 https://avatars.gith…
#> # ℹ 170 more rows
#> # ℹ 15 more variables: owner_gravatar_id <chr>, owner_url <chr>, …

# Convertir got_chars (que contiene datos de los personajes game of
# trones) en una tabla
chars <- tibble(json = got_chars)
chars
#> # A tibble: 30 × 1
#>   json             
#>   <list>           
#> 1 <named list [18]>
#> 2 <named list [18]>
#> 3 <named list [18]>
#> 4 <named list [18]>
#> 5 <named list [18]>
#> 6 <named list [18]>
#> # ℹ 24 more rows

# La columna json contiene elementos con nombre por lo que empezaremos 
# por ampliarla:
chars |> 
  unnest_wider(json) # produce:
#> # A tibble: 30 × 18
#>   url                    id name            gender culture    born           
#>   <chr>               <int> <chr>           <chr>  <chr>      <chr>          
#> 1 https://www.anapio…  1022 Theon Greyjoy   Male   "Ironborn" "In 278 AC or …
#> 2 https://www.anapio…  1052 Tyrion Lannist… Male   ""         "In 273 AC, at…
#> 3 https://www.anapio…  1074 Victarion Grey… Male   "Ironborn" "In 268 AC or …
#> 4 https://www.anapio…  1109 Will            Male   ""         ""             
#> 5 https://www.anapio…  1166 Areo Hotah      Male   "Norvoshi" "In 257 AC or …
#> 6 https://www.anapio…  1267 Chett           Male   ""         "At Hag's Mire"
#> # ℹ 24 more rows
#> # ℹ 12 more variables: died <chr>, alive <lgl>, titles <list>, …

# Se seleccionarán algunas columnas para facilitar la lectura:
characters <- chars |> 
  unnest_wider(json) |> 
  select(id, name, gender, culture, born, died, alive)
characters
#> # A tibble: 30 × 7
#>      id name              gender culture    born              died           
#>   <int> <chr>             <chr>  <chr>      <chr>             <chr>          
#> 1  1022 Theon Greyjoy     Male   "Ironborn" "In 278 AC or 27… ""             
#> 2  1052 Tyrion Lannister  Male   ""         "In 273 AC, at C… ""             
#> 3  1074 Victarion Greyjoy Male   "Ironborn" "In 268 AC or be… ""             
#> 4  1109 Will              Male   ""         ""                "In 297 AC, at…
#> 5  1166 Areo Hotah        Male   "Norvoshi" "In 257 AC or be… ""             
#> 6  1267 Chett             Male   ""         "At Hag's Mire"   "In 299 AC, at…
#> # ℹ 24 more rows
#> # ℹ 1 more variable: alive <lgl>

# Seleccionar las columnas que son columnas de listas
chars |> 
  unnest_wider(json) |> 
  select(id, where(is.list))
#> # A tibble: 30 × 8
#>      id titles    aliases    allegiances books     povBooks tvSeries playedBy
#>   <int> <list>    <list>     <list>      <list>    <list>   <list>   <list>  
#> 1  1022 <chr [2]> <chr [4]>  <chr [1]>   <chr [3]> <chr>    <chr>    <chr>   
#> 2  1052 <chr [2]> <chr [11]> <chr [1]>   <chr [2]> <chr>    <chr>    <chr>   
#> 3  1074 <chr [2]> <chr [1]>  <chr [1]>   <chr [3]> <chr>    <chr>    <chr>   
#> 4  1109 <chr [1]> <chr [1]>  <NULL>      <chr [1]> <chr>    <chr>    <chr>   
#> 5  1166 <chr [1]> <chr [1]>  <chr [1]>   <chr [3]> <chr>    <chr>    <chr>   
#> 6  1267 <chr [1]> <chr [1]>  <NULL>      <chr [2]> <chr>    <chr>    <chr>   
#> # ℹ 24 more rows

# Explorar la columna title que es una columna de lista sin nombre
chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles) # produce:
#> # A tibble: 59 × 2
#>      id titles                                              
#>   <int> <chr>                                               
#> 1  1022 Prince of Winterfell                                
#> 2  1022 Lord of the Iron Islands (by law of the green lands)
#> 3  1052 Acting Hand of the King (former)                    
#> 4  1052 Master of Coin (former)                             
#> 5  1074 Lord Captain of the Iron Fleet                      
#> 6  1074 Master of the Iron Victory                          
#> # ℹ 53 more rows

# Seleccionar id y titles y luego se desanida titles que se pasará
# a llamar title:
titles <- chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles) |> 
  filter(titles != "") |> 
  rename(title = titles)
titles
#> # A tibble: 52 × 2
#>      id title                                               
#>   <int> <chr>                                               
#> 1  1022 Prince of Winterfell                                
#> 2  1022 Lord of the Iron Islands (by law of the green lands)
#> 3  1052 Acting Hand of the King (former)                    
#> 4  1052 Master of Coin (former)                             
#> 5  1074 Lord Captain of the Iron Fleet                      
#> 6  1074 Master of the Iron Victory                          
#> # ℹ 46 more rows

# Mostrar tibble de dos columnas que contiene cinco nombres de 
# ciudades y los resultados de usar la API de geocodificación de 
# google
gmaps_cities
#> # A tibble: 5 × 2
#>   city       json            
#>   <chr>      <list>          
#> 1 Houston    <named list [2]>
#> 2 Washington <named list [2]>
#> 3 New York   <named list [2]>
#> 4 Chicago    <named list [2]>
#> 5 Arlington  <named list [2]>
# NOTA: jsones una columna de lista con nombres internos, por lo 
# que comenzamos con un unnest_wider():
gmaps_cities |> 
  unnest_wider(json) # produce:
#> # A tibble: 5 × 3
#>   city       results    status
#>   <chr>      <list>     <chr> 
#> 1 Houston    <list [1]> OK    
#> 2 Washington <list [2]> OK    
#> 3 New York   <list [1]> OK    
#> 4 Chicago    <list [1]> OK    
#> 5 Arlington  <list [2]> OK

gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results)
#> # A tibble: 7 × 2
#>   city       results         
#>   <chr>      <list>          
#> 1 Houston    <named list [5]>
#> 2 Washington <named list [5]>
#> 3 Washington <named list [5]>
#> 4 New York   <named list [5]>
#> 5 Chicago    <named list [5]>
#> 6 Arlington  <named list [5]>
#> # ℹ 1 more row
# NOTA: results también no es una lista nombrada, así que usaremos
# unnest_longer()
gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results)
#> # A tibble: 7 × 2
#>   city       results         
#>   <chr>      <list>          
#> 1 Houston    <named list [5]>
#> 2 Washington <named list [5]>
#> 3 Washington <named list [5]>
#> 4 New York   <named list [5]>
#> 5 Chicago    <named list [5]>
#> 6 Arlington  <named list [5]>
#> # ℹ 1 more row
# NOTA: Ahora results es una lista nombres por lo que se usará 
#  unnest_wider()
locations <- gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
locations
#> # A tibble: 7 × 6
#>   city       address_components formatted_address   geometry        
#>   <chr>      <list>             <chr>               <list>          
#> 1 Houston    <list [4]>         Houston, TX, USA    <named list [4]>
#> 2 Washington <list [2]>         Washington, USA     <named list [4]>
#> 3 Washington <list [4]>         Washington, DC, USA <named list [4]>
#> 4 New York   <list [3]>         New York, NY, USA   <named list [4]>
#> 5 Chicago    <list [4]>         Chicago, IL, USA    <named list [4]>
#> 6 Arlington  <list [4]>         Arlington, TX, USA  <named list [4]>
#> # ℹ 1 more row
#> # ℹ 2 more variables: place_id <chr>, types <list>

# Determinar qué hay en geometry
locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry)
#> # A tibble: 7 × 6
#>   city       formatted_address   bounds           location     location_type
#>   <chr>      <chr>               <list>           <list>       <chr>        
#> 1 Houston    Houston, TX, USA    <named list [2]> <named list> APPROXIMATE  
#> 2 Washington Washington, USA     <named list [2]> <named list> APPROXIMATE  
#> 3 Washington Washington, DC, USA <named list [2]> <named list> APPROXIMATE  
#> 4 New York   New York, NY, USA   <named list [2]> <named list> APPROXIMATE  
#> 5 Chicago    Chicago, IL, USA    <named list [2]> <named list> APPROXIMATE  
#> 6 Arlington  Arlington, TX, USA  <named list [2]> <named list> APPROXIMATE  
#> # ℹ 1 more row
#> # ℹ 1 more variable: viewport <list>

# Ver qué hay en location:
locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  unnest_wider(location)
#> # A tibble: 7 × 7
#>   city       formatted_address   bounds             lat    lng location_type
#>   <chr>      <chr>               <list>           <dbl>  <dbl> <chr>        
#> 1 Houston    Houston, TX, USA    <named list [2]>  29.8  -95.4 APPROXIMATE  
#> 2 Washington Washington, USA     <named list [2]>  47.8 -121.  APPROXIMATE  
#> 3 Washington Washington, DC, USA <named list [2]>  38.9  -77.0 APPROXIMATE  
#> 4 New York   New York, NY, USA   <named list [2]>  40.7  -74.0 APPROXIMATE  
#> 5 Chicago    Chicago, IL, USA    <named list [2]>  41.9  -87.6 APPROXIMATE  
#> 6 Arlington  Arlington, TX, USA  <named list [2]>  32.7  -97.1 APPROXIMATE  
#> # ℹ 1 more row
#> # ℹ 1 more variable: viewport <list>

# Para extraer los límites se necesitan algunos pasos más:
locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  # focus on the variables of interest
  select(!location:viewport) |>
  unnest_wider(bounds)
#> # A tibble: 7 × 4
#>   city       formatted_address   northeast        southwest       
#>   <chr>      <chr>               <list>           <list>          
#> 1 Houston    Houston, TX, USA    <named list [2]> <named list [2]>
#> 2 Washington Washington, USA     <named list [2]> <named list [2]>
#> 3 Washington Washington, DC, USA <named list [2]> <named list [2]>
#> 4 New York   New York, NY, USA   <named list [2]> <named list [2]>
#> 5 Chicago    Chicago, IL, USA    <named list [2]> <named list [2]>
#> 6 Arlington  Arlington, TX, USA  <named list [2]> <named list [2]>
#> # ℹ 1 more row

# NOTA: Luego renombramos southwesty northeast(las esquinas del 
# rectángulo) para poder usar los names_sep para crear nombres cortos 
# pero evocadores
locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  select(!location:viewport) |>
  unnest_wider(bounds) |> 
  rename(ne = northeast, sw = southwest) |> 
  unnest_wider(c(ne, sw), names_sep = "_") 
#> # A tibble: 7 × 6
#>   city       formatted_address   ne_lat ne_lng sw_lat sw_lng
#>   <chr>      <chr>                <dbl>  <dbl>  <dbl>  <dbl>
#> 1 Houston    Houston, TX, USA      30.1  -95.0   29.5  -95.8
#> 2 Washington Washington, USA       49.0 -117.    45.5 -125. 
#> 3 Washington Washington, DC, USA   39.0  -76.9   38.8  -77.1
#> 4 New York   New York, NY, USA     40.9  -73.7   40.5  -74.3
#> 5 Chicago    Chicago, IL, USA      42.0  -87.5   41.6  -87.9
#> 6 Arlington  Arlington, TX, USA    32.8  -97.0   32.6  -97.2
#> # ℹ 1 more row

# Una vez que hayas descubierto la ruta para llegar a los componentes
# que interesan , se pueden extraer directamente usando la función de
# tidyr hoist
locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  ) # produce:
# A tibble: 7 × 7
#    city       formatted_address   ne_lat sw_lat ne_lng sw_lng geometry    
#    <chr>      <chr>                <dbl>  <dbl>  <dbl>  <dbl> <list>      
#  1 Houston    Houston, TX, USA      30.1   29.5  -95.0  -95.8 <named list>
#  2 Washington Washington, USA       49.0   45.5 -117.  -125.  <named list>
#  3 Washington Washington, DC, USA   39.0   38.8  -76.9  -77.1 <named list>
#  4 New York   New York, NY, USA     40.9   40.5  -73.7  -74.3 <named list>

########################
###
### 23.4.4 Ejercicios
###
########################

# NO REALIZADOS

# El path del archivo json dentro del paquete
gh_users_json() # produce:
#> [1] "/home/runner/work/_temp/Library/repurrrsive/extdata/gh_users.json"

# Leerlo con read_json()
gh_users2 <- read_json(gh_users_json())
gh_users2 # produce: Una salida super larga

# Verificar que es lo mismo que la data que se estaba usando antes
identical(gh_users, gh_users2) # produce:
#> [1] TRUE

# Aquí hay tres conjuntos de datos JSON sencillos: primero un 
# número, luego algunos números en un array y luego ese array en 
# un objeto:
str(parse_json('1'))
#>  int 1
#>  Array: Un array es una lista con elementos sin nombre
str(parse_json('[1, 2, 3]'))
#> List of 3
#>  $ : int 1
#>  $ : int 2
#>  $ : int 3
#>  Objeto: Un objeto es una lista con elementos nombrados
str(parse_json('{"x": [1, 2, 3]}'))
#> List of 1
#>  $ x:List of 3
#>   ..$ : int 1
#>   ..$ : int 2
#>   ..$ : int 3

# Crear rectángulos con tibble(json) para que cada elemento se
# convierta en una fila
# Crear un objeto:
json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
# Crear una tibble con el objeto json
df <- tibble(json = parse_json(json))
df # produce:
#> # A tibble: 2 × 1
#>   json            
#>   <list>          
#> 1 <named list [2]>
#> 2 <named list [2]>
# Desanidar el objeto (que es una lista anidada, por eso se usa
# unnest_wider() ) 
df |> 
  unnest_wider(json) # produce:
#> # A tibble: 2 × 2
#>   name    age
#>   <chr> <int>
#> 1 John     34
#> 2 Susan    27

# Puede ser que un objeto conste de elementos nombrados únicos
# que tal vez no son exactamente iguales a elementos nombrados varios
# por lo que se tienen que asignar a una lista:
json <- '{
  "status": "OK", 
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
# Se crea una tibble listando al array
df <- tibble(json = list(parse_json(json)))
df # produce:
#> # A tibble: 1 × 1
#>   json            
#>   <list>          
#> 1 <named list [2]>
# Se desanida
df |> 
  unnest_wider(json) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
#> # A tibble: 2 × 3
#>   status name    age
#>   <chr>  <chr> <int>
#> 1 OK     John     34
#> 2 OK     Susan    27

# Esta es una alternativa para acceder a los datos directamente 
# apuntando a la parte que interesa sin necesidad de listar:
df <- tibble(results = parse_json(json)$results)
df |> 
  unnest_wider(results) # produce:
#> # A tibble: 2 × 2
#>   name    age
#>   <chr> <int>
#> 1 John     34
#> 2 Susan    27

########################
###
### 23.5.4 Ejercicios
###
########################

# NO REALIZADOS

