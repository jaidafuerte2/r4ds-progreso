#####################################
###                               ###
###         Capítulo 19           ###
###           Uniones             ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

# airlines tiene dos variables. Sin embargo es posible que carrier sea 
# un mejor identificador, lo que convierte a carrier en la clave
# principal. carrier es el código de la aeorilínea
airlines # produce:
#> # A tibble: 16 × 2
#>   carrier name                    
#>   <chr>   <chr>                   
#> 1 9E      Endeavor Air Inc.       
#> 2 AA      American Airlines Inc.  
#> 3 AS      Alaska Airlines Inc.    
#> 4 B6      JetBlue Airways         
#> 5 DL      Delta Air Lines Inc.    
#> 6 EV      ExpressJet Airlines Inc.
#> # ℹ 10 more rows

# airports tiene una variable faa que es el código identificador
# del aeropuerto, que podría ser la clave principal.
airports # produce:
#> # A tibble: 1,458 × 8
#>   faa   name                            lat   lon   alt    tz dst  
#>   <chr> <chr>                         <dbl> <dbl> <dbl> <dbl> <chr>
#> 1 04G   Lansdowne Airport              41.1 -80.6  1044    -5 A    
#> 2 06A   Moton Field Municipal Airport  32.5 -85.7   264    -6 A    
#> 3 06C   Schaumburg Regional            42.0 -88.1   801    -6 A    
#> 4 06N   Randall Airport                41.4 -74.4   523    -5 A    
#> 5 09J   Jekyll Island Airport          31.1 -81.4    11    -5 A    
#> 6 0A9   Elizabethton Municipal Airpo…  36.4 -82.2  1593    -5 A    
#> # ℹ 1,452 more rows
#> # ℹ 1 more variable: tzone <chr>

# planes registra datos sobre cada avión y tiene una variable tailnum
# o placa que podría usarse como clave principal
planes # produce:
#> # A tibble: 3,322 × 9 
#>   tailnum  year type              manufacturer    model     engines
#>   <chr>   <int> <chr>             <chr>           <chr>       <int>
#> 1 N10156   2004 Fixed wing multi… EMBRAER         EMB-145XR       2
#> 2 N102UW   1998 Fixed wing multi… AIRBUS INDUSTR… A320-214        2
#> 3 N103US   1999 Fixed wing multi… AIRBUS INDUSTR… A320-214        2
#> 4 N104UW   1999 Fixed wing multi… AIRBUS INDUSTR… A320-214        2
#> 5 N10575   2002 Fixed wing multi… EMBRAER         EMB-145LR       2
#> 6 N105UW   1999 Fixed wing multi… AIRBUS INDUSTR… A320-214        2
#> # ℹ 3,316 more rows
#> # ℹ 3 more variables: seats <int>, speed <int>, engine <chr>

# weather registra datos sobre el clima en los aeropuertos de origen.
# Cada observación se puede identificar con la combinación de 
# ubicación y hora, origin y time_hour , lo que terminaría siendo 
# su clave principal compuesta.
weather # produce:
#> # A tibble: 26,115 × 15
#>   origin  year month   day  hour  temp  dewp humid wind_dir
#>   <chr>  <int> <int> <int> <int> <dbl> <dbl> <dbl>    <dbl>
#> 1 EWR     2013     1     1     1  39.0  26.1  59.4      270
#> 2 EWR     2013     1     1     2  39.0  27.0  61.6      250
#> 3 EWR     2013     1     1     3  39.0  28.0  64.4      240
#> 4 EWR     2013     1     1     4  39.9  28.0  62.2      250
#> 5 EWR     2013     1     1     5  39.0  28.0  64.4      260
#> 6 EWR     2013     1     1     6  37.9  28.0  67.2      240
#> # ℹ 26,109 more rows
#> # ℹ 6 more variables: wind_speed <dbl>, wind_gust <dbl>, …

# CLAVE EXTERNA: Es una variable o conjunto de variables que 
# corresponden a la clave principal en otra tabla, por ejemplo:
# - flights$tailnumes una clave externa que corresponde a la 
# clave principal planes$tailnum.
# - flights$carrieres una clave externa que corresponde a la clave 
# principal airlines$carrier
# - flights$origines una clave externa que corresponde a la clave 
# principal airports$faa
# - flights$destes una clave externa que corresponde a la clave principal 
# airports$faa.
# - flights$origin- flights$time_houres una clave externa compuesta 
# que corresponde a la clave primaria compuesta 
# weather$origin - weather$time_hour

# Es importante verificar qué solo haya una clave primaria por 
# observación, para esto se puede usar count() y asegurarse que
# sólo hay una clave primaria por observación
planes |> 
  count(tailnum) |> 
  filter(n > 1) # produce:
#> # A tibble: 0 × 2 # Ningún tailnum está en más de una observación
#> # ℹ 2 variables: tailnum <chr>, n <int>
#
weather |> 
  count(time_hour, origin) |> 
  filter(n > 1) 
# produce:
#> # A tibble: 0 × 3 # Ninguna combinación time_hour - origin 
                     # está en más de una observación
#> # ℹ 3 variables: time_hour <dttm>, origin <chr>, n <int>

# También se debe verificar si hay valores faltante en las claves
# principales, sifalta un valor, no puede identificar una observación
planes |> 
  filter(is.na(tailnum)) # produce:
#> # A tibble: 0 × 9
#> # ℹ 9 variables: tailnum <chr>, year <int>, type <chr>, manufacturer <chr>,
#> #   model <chr>, engines <int>, seats <int>, speed <int>, engine <chr>
#
weather |> 
  filter(is.na(time_hour) | is.na(origin)) # produce:
#> # A tibble: 0 × 15
#> # ℹ 15 variables: origin <chr>, year <int>, month <int>, day <int>,
#> #   hour <int>, temp <dbl>, dewp <dbl>, humid <dbl>, wind_dir <dbl>, …

# En flights hay tres variables que juntas identifican de forma única
# cada vuelo:
flights |> 
  count(time_hour, carrier, flight) |> 
  filter(n > 1) # produce:
#> # A tibble: 0 × 4
#> # ℹ 4 variables: time_hour <dttm>, carrier <chr>, flight <int>, n <int>
#
# NOTA: Este último enfoque tiene algo de sentido pero podría ser
# mejor usar una CLAVE SUSTITUTA numérica utilizando el número de
# fila:
flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
flights2 # produce:
#> # A tibble: 336,776 × 20
#>      id  year month   day dep_time sched_dep_time dep_delay arr_time
#>   <int> <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1     1  2013     1     1      517            515         2      830
#> 2     2  2013     1     1      533            529         4      850
#> 3     3  2013     1     1      542            540         2      923
#> 4     4  2013     1     1      544            545        -1     1004
#> 5     5  2013     1     1      554            600        -6      812
#> 6     6  2013     1     1      554            558        -4      740
#> # ℹ 336,770 more rows
#> # ℹ 12 more variables: sched_arr_time <int>, arr_delay <dbl>, …
# Este enfoque tiene más sentido porque facilita la comunicación
# con seres humanos pues es más fácil decirle a alguien que identifique
# el vuelo 345 que identifique el UA430 que partió a las 9 de la 
# mañana del 3 de enero de 2013

########################
###
### 19.2.4 Ejercicios
###
########################

# NO REALIZADOS



# Una unión mutante permite combinar variables de dos marcos de datos.
# Primero compara las observaciones por sus claves y luego copia las 
# variables de un marco de datos a otro. Igual que mutate, las funciones 
# de unión agregan variables a la derecha. Para comprender fácil 
# se creará un conjunto de datos reducidos de sólo 6 variables
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2 # produce:
#> # A tibble: 336,776 × 6
#>    year time_hour           origin dest  tailnum carrier
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>  
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA     
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA     
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA     
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6     
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL     
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA     
#> # ℹ 336,770 more rows

# left_join() se usa mucho para añadir metadatos adicionales.
# Por ejemplo se puede agregar el nombre complaeto de la aerolínea 
# a flights2 con left_join():
flights2 |>
  left_join(airlines) # produce:
#> Joining with `by = join_by(carrier)`
#> # A tibble: 336,776 × 7
#>    year time_hour           origin dest  tailnum carrier name                
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr>               
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA      United Air Lines In…
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA      United Air Lines In…
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA      American Airlines I…
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6      JetBlue Airways     
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL      Delta Air Lines Inc.
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA      United Air Lines In…
#> # ℹ 336,770 more rows

# También podríamos averiguar la temperatura y velocidad del viento
# cuado partió cada avión:
flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))
#> Joining with `by = join_by(time_hour, origin)`
#> # A tibble: 336,776 × 8
#>    year time_hour           origin dest  tailnum carrier  temp wind_speed
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <dbl>      <dbl>
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA       39.0       12.7
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA       39.9       15.0
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA       39.0       15.0
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6       39.0       15.0
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL       39.9       16.1
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA       39.0       12.7
#> # ℹ 336,770 more rows
# NOTA: Aquí se usa origin y time_hour como claves principales de
# coinidencia entre las dos tablas

# También se puede averiguar qué tamaño de avión volaba
flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))
# produce:
#> Joining with `by = join_by(tailnum)`
#> # A tibble: 336,776 × 9
#>    year time_hour           origin dest  tailnum carrier type                
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr>               
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA      Fixed wing multi en…
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA      Fixed wing multi en…
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA      Fixed wing multi en…
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6      Fixed wing multi en…
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL      Fixed wing multi en…
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA      Fixed wing multi en…
#> # ℹ 336,770 more rows
#> # ℹ 2 more variables: engines <int>, seats <int>
# NOTA: Aquí se usa tailnum como clave principal

# Cuando left_join() no se encuentra una coincidencia para una fila 
# en x, se completan las nuevas variables con valores faltantes. 
# Por ejemplo, si no hay información sobre el avión con número de 
# cola N3ALAA, faltarán: type, engines y seats.
flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))
# produce:
#> Joining with `by = join_by(tailnum)`
#> # A tibble: 63 × 9
#>    year time_hour           origin dest  tailnum carrier type  engines seats
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr>   <int> <int>
#> 1  2013 2013-01-01 06:00:00 LGA    ORD   N3ALAA  AA      <NA>       NA    NA
#> 2  2013 2013-01-02 18:00:00 LGA    ORD   N3ALAA  AA      <NA>       NA    NA
#> 3  2013 2013-01-03 06:00:00 LGA    ORD   N3ALAA  AA      <NA>       NA    NA
#> 4  2013 2013-01-07 19:00:00 LGA    ORD   N3ALAA  AA      <NA>       NA    NA
#> 5  2013 2013-01-08 17:00:00 JFK    ORD   N3ALAA  AA      <NA>       NA    NA
#> 6  2013 2013-01-16 06:00:00 LGA    ORD   N3ALAA  AA      <NA>       NA    NA
#> # ℹ 57 more rows

# Un problema con left_join() es que de forma predeterminada se usan
# como claves de unión todas las variables que aparecen en ambas 
# tablas. Esto es un problema porque muchas veces una tabla tiene
# variables que se llaman igual pero hacen referencia a valores
# distintos.
flights2 |> 
  left_join(planes)
#> Joining with `by = join_by(year, tailnum)`
#> # A tibble: 336,776 × 13
#>    year time_hour           origin dest  tailnum carrier type  manufacturer
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr> <chr>       
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA      <NA>  <NA>        
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA      <NA>  <NA>        
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA      <NA>  <NA>        
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6      <NA>  <NA>        
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL      <NA>  <NA>        
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA      <NA>  <NA>        
#> # ℹ 336,770 more rows
#> # ℹ 5 more variables: model <chr>, engines <int>, seats <int>, …
# NOTA : En este caso hay muchos valores faltantes porque year 
# está en las dos tablas pero hacen  referencia a valores distintos
# En estos caso se debe usar join_by() para omitir que year se use
# como clave:
flights2 |> 
  left_join(planes, join_by(tailnum)) # produce:
#> # A tibble: 336,776 × 14
#>   year.x time_hour           origin dest  tailnum carrier year.y
#>    <int> <dttm>              <chr>  <chr> <chr>   <chr>    <int>
#> 1   2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA        1999
#> 2   2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA        1998
#> 3   2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA        1990
#> 4   2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6        2012
#> 5   2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL        1991
#> 6   2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA        2012
#> # ℹ 336,770 more rows
#> # ℹ 7 more variables: type <chr>, manufacturer <chr>, model <chr>, …
# NOTA: Hay que tomar en cuenta que las vriables se desambiguan con
# el subfijo "x" e "y".

# join_by(tailnum) es la abreviatura de join_by(tailnum == tailnum).
# Este tipo de unión se llama UNIÓN EQUITATIVA porque entre las dos
# tablas las claves deben ser iguales. Por ejemplo flights2 y 
# airport se pueden unir mediante dest u origin
flights2 |> 
  left_join(airports, join_by(dest == faa))
#> # A tibble: 336,776 × 13
#>    year time_hour           origin dest  tailnum carrier name                
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr>               
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA      George Bush Interco…
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA      George Bush Interco…
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA      Miami Intl          
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6      <NA>                
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL      Hartsfield Jackson …
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA      Chicago Ohare Intl  
#> # ℹ 336,770 more rows
#> # ℹ 6 more variables: lat <dbl>, lon <dbl>, alt <dbl>, tz <dbl>, …

flights2 |> 
  left_join(airports, join_by(origin == faa)) # produce:
#> # A tibble: 336,776 × 13
#>    year time_hour           origin dest  tailnum carrier name               
#>   <int> <dttm>              <chr>  <chr> <chr>   <chr>   <chr>              
#> 1  2013 2013-01-01 05:00:00 EWR    IAH   N14228  UA      Newark Liberty Intl
#> 2  2013 2013-01-01 05:00:00 LGA    IAH   N24211  UA      La Guardia         
#> 3  2013 2013-01-01 05:00:00 JFK    MIA   N619AA  AA      John F Kennedy Intl
#> 4  2013 2013-01-01 05:00:00 JFK    BQN   N804JB  B6      John F Kennedy Intl
#> 5  2013 2013-01-01 06:00:00 LGA    ATL   N668DN  DL      La Guardia         
#> 6  2013 2013-01-01 05:00:00 EWR    ORD   N39463  UA      Newark Liberty Intl
#> # ℹ 336,770 more rows
#> # ℹ 6 more variables: lat <dbl>, lon <dbl>, alt <dbl>, tz <dbl>, …

# Se puede filtrar uniones mediante semiuniones. Estas conservan todas 
# las filas x que coiniden con las y. Por ejemplo se puede usar una 
# semiunión para filtrar airports y mostrar sólo los aeropuertos de
# origen:
airports |> 
  semi_join(flights2, join_by(faa == origin))
#> # A tibble: 3 × 8
#>   faa   name                  lat   lon   alt    tz dst   tzone           
#>   <chr> <chr>               <dbl> <dbl> <dbl> <dbl> <chr> <chr>           
#> 1 EWR   Newark Liberty Intl  40.7 -74.2    18    -5 A     America/New_York
#> 2 JFK   John F Kennedy Intl  40.6 -73.8    13    -5 A     America/New_York
#> 3 LGA   La Guardia           40.8 -73.9    22    -5 A     America/New_York
#View(airports)

# O es posible mostrar sólo los aeropuertos de destino:
airports |> 
  semi_join(flights2, join_by(faa == dest))
#> # A tibble: 101 × 8
#>   faa   name                     lat    lon   alt    tz dst   tzone          
#>   <chr> <chr>                  <dbl>  <dbl> <dbl> <dbl> <chr> <chr>          
#> 1 ABQ   Albuquerque Internati…  35.0 -107.   5355    -7 A     America/Denver 
#> 2 ACK   Nantucket Mem           41.3  -70.1    48    -5 A     America/New_Yo…
#> 3 ALB   Albany Intl             42.7  -73.8   285    -5 A     America/New_Yo…
#> 4 ANC   Ted Stevens Anchorage…  61.2 -150.    152    -9 A     America/Anchor…
#> 5 ATL   Hartsfield Jackson At…  33.6  -84.4  1026    -5 A     America/New_Yo…
#> 6 AUS   Austin Bergstrom Intl   30.2  -97.7   542    -6 A     America/Chicago
#> # ℹ 95 more rows

# Las atiuniones son lo opuesto ya que devuelven todas las filas x que 
# no coinciden con y. Son útiles para encontrar valores faltantes
# implícitos. Por ejemplo podemos encontrar filas faltantes en en
# airports buscando vuelos que no tengan un aeropuerto de destino
# coincidente:
flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)
#> # A tibble: 4 × 1
#>   dest 
#>   <chr>
#> 1 BQN  
#> 2 SJU  
#> 3 STT  
#> 4 PSE
#
# O podemos encontrar cuales tailnums faltan en planes:
flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum) # produce:
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

########################
###
### 19.3.4 Ejercicios
###
########################

# NO REALIZADOS



x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
# En la unión izquierda left_join() todo lo de "x" se conserva, si
# no hay coincidencia con "y", se le asigna un valor NA. En cambio en
# la unión derecha right_join() todos los valores de "y" se conservan, 
# si no hay coincidencia con "x", se le asigna un valor NA a "x"
# En la unión completa se conservan todos los valores de "x" e "y", 
# si no hay coincidencias, se completan los valores con NA full_join().
# En la unión interna inner_join() se mantienen sólo las filas
# con claves coincidentes.
# Cuando hay más de una coincidencia por clave (de "x" en "y"), el
# valor de x se duplica. Esto suele funcionar bien, aunque el 
# siguiente caso podría ser interesante:
df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))
# Con una unión interna se combinan las claves de forma que pasamos
# de tener 3 filas a tener 5 filas (y una adevertencia)
df1 |> 
  inner_join(df2, join_by(key))
#> Warning in inner_join(df1, df2, join_by(key)): Detected an unexpected many-to-many relationship between `x` and `y`.
#> ℹ Row 2 of `x` matches multiple rows in `y`.
#> ℹ Row 2 of `y` matches multiple rows in `x`.
#> ℹ If a many-to-many relationship is expected, set `relationship =
#>   "many-to-many"` to silence this warning.
#> # A tibble: 5 × 3
#>     key val_x val_y
#>   <dbl> <chr> <chr>
#> 1     1 x1    y1   
#> 2     2 x2    y2   
#> 3     2 x2    y3   
#> 4     2 x3    y2   
#> 5     2 x3    y3

# La semi unión semi_join() conserva las coincidencias de "x" en "y"
# Mientras que la antiunión conserva las no coincidencias de "x" en 
# "y".

# Es posible mantener las claves de "x" e "y" con el argumento
# keep = TRUE
x |> inner_join(y, join_by(key == key), keep = TRUE) # produce:
#> # A tibble: 2 × 4
#>   key.x val_x key.y val_y
#>   <dbl> <chr> <dbl> <chr>
#> 1     1 x1        1 y1   
#> 2     2 x2        2 y2

# Las UNIONES CRUZADAS cruzan todas las filas. Ej:
df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)
#> # A tibble: 16 × 2
#>   name.x name.y
#>   <chr>  <chr> 
#> 1 John   John  
#> 2 John   Simon 
#> 3 John   Tracy 
#> 4 John   Max   
#> 5 Simon  John  
#> 6 Simon  Simon 
#> # ℹ 10 more rows
# Como df es un vector de 4 filas, al hacer una unión cruzada, queda
# con 16 filas

# Se puede restrigir las uniones cruzadas mediante UNIONES DE 
# DESIGUALDAD. Ej:
df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))
df |> inner_join(df, join_by(id < id)) # produce:
#> # A tibble: 6 × 4
#>    id.x name.x  id.y name.y
#>   <int> <chr>  <int> <chr> 
#> 1     1 John       2 Simon 
#> 2     1 John       3 Tracy 
#> 3     1 John       4 Max   
#> 4     2 Simon      3 Tracy 
#> 5     2 Simon      4 Max   
#> 6     3 Tracy      4 Max

# Se puede usar UNIONES RODANTES para identificar fechas cercanas,
# no exactas. Por ejemplo:
# Se cra una tabla con 4 fechas posibles para celebras cumpleaños
# por trimestre 
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)
# Ahora creamos una tabla de cumpleaños
set.seed(123) # fija resultados aleatorios  
employees <- tibble(
  # sample: escoge aleatoriamente 100 nombres y números con reemplazo
  # para que no se repitan (replace = TRUE)
  name = sample(babynames::babynames$name, 100),
  # Se suma los días a la fecha base lo que produce 100 años 
  # cumplidos a lo largo del 2022 al azar
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees # produce:
#> # A tibble: 100 × 2
#>   name     birthday  
#>   <chr>    <date>    
#> 1 Kemba    2022-01-22
#> 2 Orean    2022-06-26
#> 3 Kirstyn  2022-02-11
#> 4 Amparo   2022-11-11
#> 5 Belen    2022-03-25
#> 6 Rayshaun 2022-01-11
#> # ℹ 94 more rows
#
# Y para cada empleado, queremos encontrar la última fecha de 
# fiesta anterior o igual a su cumpleaños. Podemos expresarlo con 
# una unión continua closest():
employees |> 
  left_join(parties, join_by(closest(birthday >= party)))
# produce:
#> # A tibble: 100 × 4
#>   name     birthday       q party     
#>   <chr>    <date>     <int> <date>    
#> 1 Kemba    2022-01-22     1 2022-01-10
#> 2 Orean    2022-06-26     2 2022-04-04
#> 3 Kirstyn  2022-02-11     1 2022-01-10
#> 4 Amparo   2022-11-11     4 2022-10-03
#> 5 Belen    2022-03-25     1 2022-01-10
#> 6 Rayshaun 2022-01-11     1 2022-01-10
#> # ℹ 94 more rows

