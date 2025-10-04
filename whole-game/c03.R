#####################################
###                               ###
###         Capítulo 3            ###
###   Transformación de datos     ### 
###                               ###
#####################################

library(nycflights13)
library(tidyverse)
?flights
View(flights)

# |> : pasa un abjeto de su parte izquierda como argumento de la
# primera función de la parte derecha
# Encontrar todos los vuelos que salieron con más de 120 minutoss de 
# retraso:
flights |>
  filter(dep_delay > 120) # produce: 9723 filas y 19 columnas
## A tibble: 9,723 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      848           1835       853     1001
#2  2013     1     1      957            733       144     1056
#3  2013     1     1     1114            900       134     1447
#4  2013     1     1     1540           1338       122     2020

# Vuelos  que salieron el primero de Enero:
flights |>
  filter(month == 1 & day == 1) # produce:
## A tibble: 842 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Vuelos que salieron en Enero o Febrero
flights |> 
  filter(month == 1 | month == 2) # produce:
## A tibble: 51,955 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Se puede usar un atajo para evitar usar un operador o para el mismo
# nombre de la columna con el operador %in% y un vector
flights |>
  filter(month %in% c(1, 2)) # produce:
## A tibble: 51,955 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# NOTA: la función filter crea un nuevo marco de datos y lo imprime.
# filter no cambia el flights original. Para guardar el resultado
# se debe usar el operador de asignación <- :
# Por ejemplo si quiero guardar las filas de los vuelos que salieron 
# el primero de Enero:
jan1 <- flights |> 
  filter(month == 1 & day == 1)
jan1 # produce:
## A tibble: 842 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
#View(jan1) # Abre una nueva pestaña con todas las filas de registros
# de jan1

# el siguiente código ordena por la hora de salida, que se distribuye 
# en cuatro columnas. Primero se obtienen los años más antiguos, luego 
# los del mismo año, los meses más antiguos, etc.
flights |> 
  arrange(year, month, day, dep_time) # produce:
## A tibble: 336,776 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923

#  el siguiente código ordena por la hora de salida, que se distribuye 
# en cuatro columnas. Primero se obtienen los años más antiguos, 
# luego los del mismo año, los meses más antiguos, etc.

# ordena los vuelos de mayor a menor retraso
flights |> 
  arrange(desc(dep_delay)) # produce:
## A tibble: 336,776 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     9      641            900      1301     1242
#2  2013     6    15     1432           1935      1137     1607
#3  2013     1    10     1121           1635      1126     1239
#4  2013     9    20     1139           1845      1014     1457

# Encuentra todas las filas únicas de un conjunto de datos. Es decir
# si hay dos filas exctamente iguales. sólo se muestra 1
flights |>
  distinct() # produce:
## A tibble: 336,776 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Encontrar todos los pares origin-destination únicos
flights |>
  distinct(origin, dest) # produce:
## A tibble: 224 × 2
# origin dest 
# <chr>  <chr>
#1 EWR    IAH  
#2 LGA    IAH  
#3 JFK    MIA  
#4 JFK    BQN 

# Encontrar todos los pares origin-destination únicos, pero conservando 
# el resto de columnas
flights |>
  distinct(origin, dest, .keep_all = TRUE) # produce:
## A tibble: 224 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
# No es una coincidencia que todos estos vuelos distintos sean el 1 
# de enero: distinct()encontrará la primera aparición de una fila 
# única en el conjunto de datos y descartará el resto.

# Encontrar el número de coincidencias de pares origin-destination
flights |> 
  count(origin, dest, sort = TRUE) # produce:
## A tibble: 224 × 3
# origin dest      n
#  <chr>  <chr> <int>
#1 JFK    LAX   11262
#2 LGA    ATL   10263
#3 LGA    ORD    8857
#4 JFK    SFO    8204

########################
###
### 3.2.5 Ejercicios
###
########################

# 1. En una única tubería para cada condición, encuentre todos los 
# vuelos que cumplan la condición:
#  
# - Tuvo un retraso de llegada de dos o más horas
# - Voló a Houston ( IAHo HOU)
# - Fueron operados por United, American o Delta
# - Salida en verano (julio, agosto y septiembre)
# - Llegó más de dos horas tarde pero no se fue tarde.
# - Tuvimos un retraso de al menos una hora, pero recuperamos más 
# - de 30 minutos en vuelo.
?flights
flights |>
  filter(arr_delay > 2 & (dest == "IAH"| dest == "HOU") &
           (carrier == "UA" | carrier == "AA" | carrier == "DL") & 
           (month == 7 | month == 8 | month == 9) & 
           dep_delay <= 0 &
           (dep_delay > 0 & arr_delay <= -30)
)
# Tuvo un retraso de llegada de dos o más horas
flights |>
  filter(arr_delay >= 120)
# Voló a Houston ( IAHo HOU)
flights |>
  filter(dest == "IAH" | dest == "HOU")
# Fueron operados por United, American o Delta
flights |>
  filter(carrier %in% c("UA", "AA", "DL"))
# Salida en verano (julio, agosto y septiembre)
flights |>
  filter(month %in% c(7, 8, 9))
# Llegó más de dos horas tarde pero no se fue tarde.
flights |>
  filter(arr_delay >= 120 & dep_delay <= 0) #|> view()
# Tuvimos un retraso de al menos una hora, pero recuperamos más de 
# 30 minutos en vuelo.
flights |>
  filter(dep_delay >= 60 & (dep_delay + arr_delay) < -30)

# 2. Ordena flights para encontrar los vuelos con mayor retraso. 
# Encuentra los vuelos que salieron más temprano por la mañana.
flights |>
  arrange(desc(dep_delay)) # produce:
## A tibble: 336,776 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     9      641            900      1301     1242
#2  2013     6    15     1432           1935      1137     1607
#3  2013     1    10     1121           1635      1126     1239
#4  2013     9    20     1139           1845      1014     1457
flights |>
  arrange(dep_time) # produce:
## A tibble: 336,776 × 19
#year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1    13        1           2249        72      108
#2  2013     1    31        1           2100       181      124
#3  2013    11    13        1           2359         2      442
#4  2013    12    16        1           2359         2      447

# 3. Ordena flights para encontrar los vuelos más rápidos. 
# (Sugerencia: Intenta incluir un cálculo matemático en tu función)
flights|> 
  arrange(desc(distance / air_time)) # |> View()
# Se puede usar la función relocate para reposicionar en las primeras
# columnas distance y air_time
flights|> 
  arrange(desc(distance / air_time)) |> relocate(distance, air_time)
## A tibble: 336,776 × 19
#   distance air_time  year month   day dep_time sched_dep_time dep_delay
#      <dbl>    <dbl> <int> <int> <int>    <int>          <int>     <dbl>
#1      762       65  2013     5    25     1709           1700         9
#2     1008       93  2013     7     2     1558           1513        45
#3      594       55  2013     5    13     2040           2025        15
#4      748       70  2013     3    23     1914           1910         4


# 4. ¿Hubo un vuelo todos los días del año 2013?
flights |>
  distinct(month, day) # |> View()
# produce:
## A tibble: 365 × 2
#    month   day
#    <int> <int>
#1     1     1
#2     1     2
#3     1     3
#4     1     4
# Si hubo un vuelo todos los días del año 2013 porque se 
# se encontraron 365 filas idénticas de pares month - day

# 5. ¿Qué vuelos recorrieron la mayor distancia? ¿Cuáles recorrieron 
# la menor distancia?
# La mayor distancia
flights |> 
  arrange(desc(distance)) |> distinct(distance) #|> View()
# produce:
## A tibble: 214 × 1
#   distance
#      <dbl>
#1     4983
#2     4963
#3     3370
#4     2586
flights |> 
  arrange(distance) |> distinct(distance)
# produce:
## A tibble: 214 × 1
#   distance
#      <dbl>
#1       17
#2       80
#3       94
#4       96
flights |> 
  arrange(desc(distance)) |> relocate(distance) # produce:
## A tibble: 336,776 × 19
#   distance  year month   day dep_time sched_dep_time dep_delay arr_time
#      <dbl> <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1     4983  2013     1     1      857            900        -3     1516
#2     4983  2013     1     2      909            900         9     1525
#3     4983  2013     1     3      914            900        14     1504
#4     4983  2013     1     4      900            900         0     1516
# NOTA: Lo que hace relocate es reposicionar en la primera columna
# una columna
flights |> 
  arrange(distance) |> relocate(distance)
# produce:
## A tibble: 336,776 × 19
#   distance  year month   day dep_time sched_dep_time dep_delay arr_time
#      <dbl> <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1       17  2013     7    27       NA            106        NA       NA
#2       80  2013     1     3     2127           2129        -2     2222
#3       80  2013     1     4     1240           1200        40     1333
#4       80  2013     1     4     1829           1615       134     1937

# 6. ¿Importa el orden que usaste filter()y arrange()si usas ambos? 
# ¿Por qué sí o por qué no? Piensa en los resultados y en cuánto 
# trabajo tendrían que realizar las funciones.
# RESPUESTA: Sí importa mucho el orden porque si primero se ordena
# y luego se filtra, se oredena todo y luego se filtra todo. En cambio
# si primero filtro, se filtra todo pero luego se ordena sólo la
# parte filtrada, lo que es más eficiente.

# mutate(): agrega nuevas columnas a partir de existentes
# Crea dos columnas extras, gain que nos dice cuanto tiempo un vuelo 
# retrasado recorrió en el aire y speed que calcula la velocidad
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  ) |> relocate(gain, speed, dep_delay, arr_delay) # produce:
## A tibble: 336,776 × 21
#    gain speed dep_delay arr_delay  year month   day dep_time
#   <dbl> <dbl>     <dbl>     <dbl> <int> <int> <int>    <int>
#1    -9  370.         2        11  2013     1     1      517
#2   -16  374.         4        20  2013     1     1      533
#3   -31  408.         2        33  2013     1     1      542
#4    17  517.        -1       -18  2013     1     1      544

# Para traer al inicio a las nuevas columnas se puede usar .before
# en lugar de relocate
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  ) # produce:
## A tibble: 336,776 × 21
#    gain speed  year month   day dep_time sched_dep_time dep_delay
#   <dbl> <dbl> <int> <int> <int>    <int>          <int>     <dbl>
#1    -9  370.  2013     1     1      517            515         2
#2   -16  374.  2013     1     1      533            529         4
#3   -31  408.  2013     1     1      542            540         2
#4    17  517.  2013     1     1      544            545        -1
# NOTA: el punto (.) indica que .before es el nombre de un argumento
# de mutate no el nombre de una nueva variable creada.

# Es posible poner una columna en una posición específica con
# .before o .after ;
# Por ejemplo de esta forma se pueden poner las columnas gain y speed
# después de la columna day
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )
## A tibble: 336,776 × 21
#   year month   day  gain speed dep_time sched_dep_time dep_delay
#  <int> <int> <int> <dbl> <dbl>    <int>          <int>     <dbl>
#1  2013     1     1    -9  370.      517            515         2
#2  2013     1     1   -16  374.      533            529         4
#3  2013     1     1   -31  408.      542            540         2
#4  2013     1     1    17  517.      544            545        -1

# Se pueden mantener en la tabla sólo las columnas involucradas dentro
# de mutate con el argumento .keep establecido en "used".
# Por ejemplo, la siguiente salida contendrá solo las variables 
# dep_delay, arr_delay, air_time, gain, hoursy gain_per_hour. 
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time * 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  ) # produce:
## A tibble: 336,776 × 6
#   dep_delay arr_delay air_time  gain hours gain_per_hour
#       <dbl>     <dbl>    <dbl> <dbl> <dbl>         <dbl>
#1         2        11      227    -9 13620     -0.000661
#2         4        20      227   -16 13620     -0.00117 
#3         2        33      160   -31  9600     -0.00323 
#4        -1       -18      183    17 10980      0.00155 

# select(): es una función que permite mostrar sólo ciertas columnas
# seleccionadas

# Seleccionar columnas por nombre:
flights |>
  select(year, month, day) # produce:
## A tibble: 336,776 × 3
#   year month   day
#  <int> <int> <int>
#1  2013     1     1
#2  2013     1     1
#3  2013     1     1
#4  2013     1     1

# Seleccionar todas las columnas entre año y día (inclusive):
flights |> 
  select(year:day) #produce:
## A tibble: 336,776 × 3
#   year month   day
#  <int> <int> <int>
#1  2013     1     1
#2  2013     1     1
#3  2013     1     1
#4  2013     1     1

# Seleccionar todas las columnas excepto las de año a día (inclusive)
flights |>
  select(!year:day) # produce:
## A tibble: 336,776 × 16
#   dep_time sched_dep_time dep_delay arr_time sched_arr_time arr_delay
#      <int>          <int>     <dbl>    <int>          <int>     <dbl>
#1      517            515         2      830            819        11
#2      533            529         4      850            830        20
#3      542            540         2      923            850        33
#4      544            545        -1     1004           1022       -18

# Seleccionar todas las columnas que sean caracteres:
flights |> 
  select(where(is.character)) # produce:
## A tibble: 336,776 × 4
# carrier tailnum origin dest 
# <chr>   <chr>   <chr>  <chr>
#1 UA      N14228  EWR    IAH  
#2 UA      N24211  LGA    IAH  
#3 AA      N619AA  JFK    MIA  
#4 B6      N804JB  JFK    BQN  

# Hay una serie de funciones auxiliares que puedes utilizar dentro de select():
#  
# - starts_with("abc"): coincide con nombres que comienzan con “abc”.
# - ends_with("xyz"): coincide con nombres que terminan con “xyz”.
# - contains("ijk"): coincide con nombres que contienen “ijk”.
# - num_range("x", 1:3):coincidencias x1, x2y x3.
# Consulte ?select para obtener más detalles.
?select

# Se pueden seleccionar variables para renombrarlas con select, de 
# esta forma:
flights |>
  select(tail_num = tailnum) # produce:
## A tibble: 336,776 × 1
#   tail_num
#   <chr>   
#1 N14228  
#2 N24211  
#3 N619AA  
#4 N804JB
# De esta forma se cambia el nombre tailnum a tail_num

# rename() : se usa para mostrar una tabla con el nombre de ciertas
# columnas cambiado:
flights |>
  rename(tail_num = tailnum) |> relocate(tail_num) # produce:
## A tibble: 336,776 × 19
#tail_num  year month   day dep_time sched_dep_time dep_delay arr_time
#   <chr>    <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1 N14228    2013     1     1      517            515         2      830
#2 N24211    2013     1     1      533            529         4      850
#3 N619AA    2013     1     1      542            540         2      923
#4 N804JB    2013     1     1      544            545        -1     1004

# relocate() : se usa para poner ciertas columnas al inicio
# Por ejemplo para poner time_hour y air_time al inicio
flights |>
  relocate(time_hour, air_time)
## A tibble: 336,776 × 19
#time_hour           air_time  year month   day dep_time sched_dep_time
#   <dttm>                 <dbl> <int> <int> <int>    <int>          <int>
#1 2013-01-01 05:00:00      227  2013     1     1      517            515
#2 2013-01-01 05:00:00      227  2013     1     1      533            529
#3 2013-01-01 05:00:00      160  2013     1     1      542            540
#4 2013-01-01 05:00:00      183  2013     1     1      544            545

# También se puede especificar donde poner las variables con los
# argumentos . before y .after
flights |>
  relocate(year:dep_time, .after = time_hour)
## A tibble: 336,776 × 19
#   sched_dep_time dep_delay arr_time sched_arr_time arr_delay carrier
#            <int>     <dbl>    <int>          <int>     <dbl> <chr>  
#1            515         2      830            819        11 UA     
#2            529         4      850            830        20 UA     
#3            540         2      923            850        33 AA     
#4            545        -1     1004           1022       -18 B6     
flights |> 
  relocate(starts_with("arr"), .before = dep_time) # produce:
## A tibble: 336,776 × 19
#   year month   day arr_time arr_delay dep_time sched_dep_time dep_delay
#  <int> <int> <int>    <int>     <dbl>    <int>          <int>     <dbl>
#1  2013     1     1      830        11      517            515         2
#2  2013     1     1      850        20      533            529         4
#3  2013     1     1      923        33      542            540         2
#4  2013     1     1     1004       -18      544            545        -1

########################
###
### 3.3.5 Ejercicios
###
########################

# 1. Compara dep_time, sched_dep_time y dep_delay. ¿Cómo esperarías 
# que se relacionaran esos tres números?
# sched_dep_time : es el horario programado de salida del avión.
# dep_time : es la hora en la que realmente salió
# dep_delay : es el timpo de retraso que sería una resta 
# (dep_time - sched_dep_time)

# 2. Piense en tantas formas como sea posible para seleccionar 
# dep_time, dep_delay, arr_time, y arr_delay de flights.
flights |>
  select(dep_time, dep_delay, arr_time, arr_delay) # produce:
# A tibble: 336,776 × 4
#   dep_time dep_delay arr_time arr_delay
#      <int>     <dbl>    <int>     <dbl>
#1      517         2      830        11
#2      533         4      850        20
#3      542         2      923        33
#4      544        -1     1004       -18
flights |>
  select(dep_time, dep_delay:arr_time, arr_delay) # produce:
# A tibble: 336,776 × 4
#   dep_time dep_delay arr_time arr_delay
#      <int>     <dbl>    <int>     <dbl>
#1      517         2      830        11
#2      533         4      850        20
#3      542         2      923        33
#4      544        -1     1004       -18
flights |>
  relocate(dep_time, dep_delay, arr_time, arr_delay) |>
  select(dep_time:arr_delay) # produce: 
## A tibble: 336,776 × 4
#   dep_time dep_delay arr_time arr_delay
#      <int>     <dbl>    <int>     <dbl>
#1      517         2      830        11
#2      533         4      850        20
#3      542         2      923        33
#4      544        -1     1004       -18

# 3. ¿Qué sucede si especifica el nombre de la misma variable varias 
# veces en una select() llamada?
flights |> 
  select(dep_time, dep_time, dep_time) # produce:
## A tibble: 336,776 × 1
#   dep_time
#      <int>
#1      517
#2      533
#3      542
#4      544
# Se imprime sólo una vez
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> 
  select(any_of(variables)) # produce:
## A tibble: 336,776 × 5
#    year month   day dep_delay arr_delay
#   <int> <int> <int>     <dbl>     <dbl>
#1  2013     1     1         2        11
#2  2013     1     1         4        20
#3  2013     1     1         2        33
#4  2013     1     1        -1       -18
flights |>
  select(variables) # produce:
## A tibble: 336,776 × 5
#    year month   day dep_delay arr_delay
#   <int> <int> <int>     <dbl>     <dbl>
#1  2013     1     1         2        11
#2  2013     1     1         4        20
#3  2013     1     1         2        33
#4  2013     1     1        -1       -183
# No entiendo bien cual es la finalidad de usar any_of(), parece que
# produce lo mismo que un select clásico

# 5. ¿Te sorprende el resultado de ejecutar el siguiente código? 
# ¿Cómo gestionan los asistentes de selección las mayúsculas y 
# minúsculas por defecto? ¿Cómo puedes cambiar ese valor 
# predeterminado?
flights |>
  select(contains("TIME"))
# Si me sorprende porque "TIME" Y "...time..." no es exactamente lo
# mismo
?contains()
flights |>
  select(contains("TIME", ignore.case = FALSE)) # produce:
# A tibble: 336,776 × 0
# Use `print(n = ...)` to see more rows
# Creo que puedo cambiar ese valor predeterminado con el argumento 
# ignore.case

# 6. Cambie el nombre air_time a air_time_min para indicar las unidades 
# de medida y moverlo al comienzo del marco de datos.
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min) # produce:
# A tibble: 336,776 × 19
#   air_time_min  year month   day dep_time sched_dep_time dep_delay
#          <dbl> <int> <int> <int>    <int>          <int>     <dbl>
#1          227  2013     1     1      517            515         2
#2          227  2013     1     1      533            529         4
#3          160  2013     1     1      542            540         2
#4          183  2013     1     1      544            545        -1

# 7. ¿Por qué no funciona lo siguiente y qué significa el error?
#flights |> 
#  select(tailnum) |> 
#  arrange(arr_delay)
#> Error in `arrange()`:
#>  In argument: `..1 = arr_delay`.
#> Caused by error:
#> ! object 'arr_delay' not found
# No sé por qué no funciona pero intuyo que es porque a arrange se
# le envía una sóla columna en lugar de la tabla entera. Esa columna
# sólo tiene a tailnum y no contiene ninguna otra columna, tampoco a
# arr_delay

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed)) # produce:
# A tibble: 7,198 × 7
#    year month   day dep_time carrier flight speed
#   <int> <int> <int>    <int> <chr>    <int> <dbl>
#1  2013     7     9      707 UA         226  522.
#2  2013     8    27     1850 UA        1128  521.
#3  2013     8    28      902 UA        1711  519.
#4  2013     8    28     2122 UA        1022  519.

# %>% : Es la tubería igual a "|>" que se escribe con el comando
# CTRL-SHIFT-M . %>% 

# Agrupar por mes
flights |> 
  group_by(month) # produce:
# A tibble: 336,776 × 19
# Groups:   month [12]
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
# NOTA: Lo que hace group_by es agrupar las filas por meses:
# "Groups:   month [12]". group_by añade esta característica de 
# agrupación (denominada clase) al marco de datos

# summarize(): resume los grupos de group_by en una sola fila para
# cada grupo:
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  ) # produce:
# A tibble: 12 × 2
#   month avg_delay
#   <int>     <dbl>
#1     1        NA
#2     2        NA
#3     3        NA
#4     4        NA
# NOTA: NA significa valor faltante y se produce porque algunas filas
# de dep_delay no tienen el valor de dep_delay entonces al sacar su
# promedio se produce un valor NA. Para solucionar esto se debe hacer
# algo extra:
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
)
# A tibble: 12 × 3
#month avg_delay     n
#   <int>     <dbl> <int>
#1     1     10.0  27004
#2     2     10.8  24951
#3     3     13.2  28834
#4     4     13.9  28330
# NOTA: na.rm = TRUE ignora todos los valores faltantes en la columna
# dep_delay y n() devuelve el número de filas en cada grupo

# busca los vuelos con más retraso al llegar al destino
flights |> 
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest) # produce:
# A tibble: 108 × 19
# Groups:   dest [105]
#    dest   year month   day dep_time sched_dep_time dep_delay arr_time
#   <chr> <int> <int> <int>    <int>          <int>     <dbl>    <int>
# 1 ABQ    2013     7    22     2145           2007        98      132
# 2 ACK    2013     7    23     1139            800       219     1250
# 3 ALB    2013     1    25      123           2000       323      229
# 4 ANC    2013     8    17     1740           1625        75     2042
# NOTA: hay 105 grupos pero 108 filas esto es porque n = 1 significa
# obtener todas las filas con el valor más alto y en algunos casos
# los valores más altos (por grupo) se repiten 1 o varias veces.
# Si se desea solo 1 fila por grupos se debe usar el argumento 
# with_ties = FALSE

# Crear grupos para cada fecha
daily <- flights |>
  group_by(year, month, day)
daily # produce:
# A tibble: 336,776 × 19
# Groups:   year, month, day [365]
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Crear un resumen del número de filas por cada una de las 365 fechas
# agrupadas por año, mes  y día
daily_flights <- daily |>
  summarize(n = n()) # produce:
# summarise() has grouped output by 'year', 'month'. You can override
# using the .groups argument.
  
# Crear un resumen del número de filas por cada una de las 365 fechas
# agrupadas por año, mes  y día
daily_flights <- daily |>
  summarize(n = n(),
  .groups = "drop_last" # Para que no produzca el mensaje anterior
  )                     # sin el argumento .groups
# produce: nothing 

daily_flights # produce:
# A tibble: 365 × 4
# Groups:   year, month [12]
#    year month   day     n
#   <int> <int> <int> <int>
#1  2013     1     1   842
#2  2013     1     2   943
#3  2013     1     3   914
#4  2013     1     4   915

# desagrupar un marco de datos agrupado
daily |> ungroup() # produce:
# A tibble: 336,776 × 19
#    year month   day dep_time sched_dep_time dep_delay arr_time
#   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Resumir un marco de datos no agrupado
daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  ) # produce:
## A tibble: 1 × 2
#     avg_delay flights
#         <dbl>   <int>
#  1      12.6  336776

# Crea un resumen mensual de promedios de retrasos de salida y cantidad
# de filas
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  ) # produce:
# A tibble: 12 × 3
#  month delay     n
#  <int> <dbl> <int>
#1     1 10.0  27004
#2    10  6.24 28889
#3    11  5.44 27268
#4    12 16.6  28135

# Crea un resumen de promedios de retrasos de salida y cantidad de
# filas por pares origen - destino
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  ) # produce:
# A tibble: 224 × 4
#origin dest  delay     n
#  <chr>  <chr> <dbl> <int>
#1 EWR    IAH   11.8   3973
#2 LGA    IAH    9.06  2951
#3 JFK    MIA    9.34  3314
#4 JFK    BQN    6.67   599

########################
###
### 3.5.7 Ejercicios
###
########################

# 1. ¿Qué aerolínea tiene los peores retrasos promedio? Desafío: 
# ¿Puedes desentrañar los efectos de los malos aeropuertos frente 
# a los de las malas aerolíneas? ¿Por qué sí o por qué no? (Pista: 
# piensa en... flights |> group_by(carrier, dest) |> summarize(n()))

# Agrupar por aerolínea y conocer el promedio de retraso de salida y
# llegada de cada aerolínea
flights |> 
  group_by(carrier) |>
  summarize(
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(avg_dep_delay))
# produce: 
# A tibble: 16 × 4
#  carrier avg_dep_delay avg_arr_delay     n
#  <chr>           <dbl>         <dbl> <int>
#1 F9              20.2         21.9     685
#2 EV              20.0         15.8   54173
#3 YV              19.0         15.6     601
#4 FL              18.7         20.1    3260
#5 WN              17.7          9.65  12275
#6 9E              16.7          7.38  18460

# 2. Encuentra los vuelos que sufren mayor retraso en la salida 
# desde cada destino.
flights |>
  group_by(dest) |>
  arrange(dest, desc(dep_delay)) |>
  relocate(dest, dep_delay) 
# A tibble: 336,776 × 19
#   dest  dep_delay  year month   day dep_time sched_dep_time arr_time
#   <chr>     <dbl> <int> <int> <int>    <int>          <int>    <int>
#1 ABQ         142  2013    12    14     2223           2001      133
#2 ABQ         139  2013    12    17     2220           2001      120
#3 ABQ         125  2013     7    30     2212           2007       57
#4 ABQ         125  2013     9     2     2212           2007       48

# 3 ¿Cómo varían los retrasos a lo largo del día? Ilustre su 
# respuesta con un gráfico.

flights |> 
  group_by(hour) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_dep_delay)) +
  geom_point() +
  geom_smooth()

# 4. ¿Qué pasa si le das un negativo n a ti slice_min()y a tus amigos?
# Agrupar por destino y mostrar los que tuvieron menos retraso de
# llegada
flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = 1) |>
  relocate(dest, arr_delay) # produce:
# A tibble: 115 × 19
# Groups:   dest [105]
#   dest  arr_delay  year month   day dep_time sched_dep_time dep_delay
#  <chr>     <dbl> <int> <int> <int>    <int>          <int>     <dbl>
#1 ABQ         -61  2013    11    13     1953           2000        -7
#2 ACK         -25  2013     8    11      751            800        -9
#3 ALB         -34  2013     6    12      738            746        -8
#4 ANC         -47  2013     7    27     1617           1615         2
# Parece que lo que hace es agrupar en forma ascendente según el
# restraso de llegada y por grupos, no es lo mismo que n = 1
flights |>
  group_by(dest) |>
  slice_min(arr_delay, n = -1) |>
  relocate(dest, arr_delay) # produce:
# A tibble: 336,762 × 19
# Groups:   dest [103]
#   dest  arr_delay  year month   day dep_time sched_dep_time dep_delay
#  <chr>     <dbl> <int> <int> <int>    <int>          <int>     <dbl>
#1 ABQ         -61  2013    11    13     1953           2000        -7
#2 ABQ         -58  2013     5    14     1958           2001        -3
#3 ABQ         -56  2013     6     4     1957           2001        -4

# NOTA: La diferencia se ve mejor en estos casos sin agrupar:
# Se ordena en orden ascendente según su retraso de salida y se 
# muestran todas las filas de la tabla 336776
flights |> 
  slice_min(dep_delay, n = -5) |>
  relocate(dep_delay)
#> # A tibble: 336,776 × 19
#>   dep_delay  year month   day dep_time sched_dep_time arr_time sched_arr_time
#>       <dbl> <int> <int> <int>    <int>          <int>    <int>          <int>
#> 1       -43  2013    12     7     2040           2123       40           2352
#> 2       -33  2013     2     3     2022           2055     2240           2338
#> 3       -32  2013    11    10     1408           1440     1549           1559
#> 4       -30  2013     1    11     1900           1930     2233           2243
#> 5       -27  2013     1    29     1703           1730     1947           1957
#> 6       -26  2013     8     9      729            755     1002            955
#> #  336,770 more rows
# Se ordena la tabla en orden ascendente según su retraso de salida
# pero se muestran sólo las 5 primeras filas de la tabla
flights |> 
  slice_min(dep_delay, n = 5) |>
  relocate(dep_delay)
#> # A tibble: 5 × 19
#>   dep_delay  year month   day dep_time sched_dep_time arr_time sched_arr_time
#>       <dbl> <int> <int> <int>    <int>          <int>    <int>          <int>
#> 1       -43  2013    12     7     2040           2123       40           2352
#> 2       -33  2013     2     3     2022           2055     2240           2338
#> 3       -32  2013    11    10     1408           1440     1549           1559
#> 4       -30  2013     1    11     1900           1930     2233           2243
#> 5       -27  2013     1    29     1703           1730     1947           1957

# 5. Explica qué count()hace en términos de los verbos dplyr que 
# acabas de aprender. ¿Qué hace el sortargumento count()?
?count
flights |>
  count(origin, dest, sort = TRUE) # produce:
# A tibble: 224 × 3
# origin dest      n
# <chr>  <chr> <int>
#1 JFK    LAX   11262
#2 LGA    ATL   10263
#3 LGA    ORD    8857
#4 JFK    SFO    8204
#5 LGA    CLT    6168
#6 EWR    ORD    6100
# Usar la función count() con el argumento sort = TRUE lo que hace
# es contar las ocurrencias y ordenar en forma descendente

# 6. Supongamos que tenemos el siguiente pequeño marco de datos:
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)

# 6.a. Escribe cómo crees que se verá el resultado, luego comprueba 
# si estabas en lo correcto y describe qué group_by()es.
# Respuesta: Pienso que se verán tres columnas (x, y y z)

df # produce:
#function (x, df1, df2, ncp, log = FALSE) 
#{
#  if (missing(ncp)) 
#    .Call(C_df, x, df1, df2, log)
#  else .Call(C_dnf, x, df1, df2, ncp, log)
#}
#<bytecode: 0x6219bf95dc18>
#  <environment: namespace:stats>

df |> 
  group_by(y) # produce:
# A tibble: 5 × 3
# Groups:   y [2]
#      x y     z    
#  <int> <chr> <chr>
#1     1 a     K    
#2     2 b     K    
#3     3 a     L    
#4     4 a     L    
#5     5 b     K 

# 6.b. Escribe cómo crees que se verá el resultado, comprueba si 
# acertaste y describe qué arrange()es. Además, comenta en qué se 
# diferencia del group_by()punto (a).
df |>
  arrange(y)
# Respuesta, no tengo idea porque arrange ordena valores numéricos
# y y tiene caracteres
# produce:
# A tibble: 5 × 3
#      x y     z    
#  <int> <chr> <chr>
#1     1 a     K    
#2     3 a     L    
#3     4 a     L    
#4     2 b     K    
#5     5 b     K  

# 6.c Escribe cómo crees que se verá el resultado, luego verifica si 
# estabas en lo correcto y describe lo que hace la canalización.
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
# Respuesta: Pienso que aparecerán sólo dos filas, una para a y otra
# para b. Ademas habrá una nueva columna para mean_x; que será  para
# a algo cercano a 2 y para b algo cercanoa 3 o 4
# produce: 
# A tibble: 2 × 2
#   y     mean_x
#  <chr>  <dbl>
#1 a       2.67
#2 b       3.5 

# Escribe cómo crees que se verá el resultado, luego verifica si 
# estabas en lo correcto y describe lo que hace la canalización.
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
# Resuesta: Se obtienen dos columnas con los valor de y y z. Además
# se crea una nueva columna para mean:x
#produce:
# `summarise()` has grouped output by 'y'. You can override using the
# `.groups` argument.
# A tibble: 3 × 3
# Groups:   y [2]
#  y     z     mean_x
#  <chr> <chr>  <dbl>
#1 a     K        1  
#2 a     L        3.5
#3 b     K        3.5

# 6.e. Escribe cómo crees que se verá el resultado, comprueba si 
# acertaste y describe qué hace la canalización. ¿En qué se 
# diferencia el resultado del del apartado (d)?
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")
# Respuesta: Ingual que en el caso de 6.e se mostrará una columna
# y y una columna z con los valores de x. Además se mostrará una
# columna para mean_x. La diferencia con 6.e es el .groups = "drop"
# que, a mi parecer quita el mensaje de summarize.
# produce:
# A tibble: 3 × 3
#  y     z     mean_x
# <chr> <chr>  <dbl>
#1 a     K        1  
#2 a     L        3.5
#3 b     K        3.5

# 6.f. Escribe cómo crees que se verán los resultados, comprueba si 
# acertaste y describe la función de cada canal. ¿En qué se 
# diferencian los resultados de ambos canales?

df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x)) # produce:
#`summarise()` has grouped output by 'y'. You can override using the
#`.groups` argument.
# A tibble: 3 × 3
# Groups:   y [2]
#  y     z     mean_x
#  <chr> <chr>  <dbl>
#1 a     K        1  
#2 a     L        3.5
#3 b     K        3.5

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x)) # produce:
# A tibble: 5 × 4
# Groups:   y, z [3]
#      x y     z     mean_x
#  <int> <chr> <chr>  <dbl>
#1     1 a     K        1  
#2     2 b     K        3.5
#3     3 a     L        3.5
#4     4 a     L        3.5
#5     5 b     K        3.5

# Respuesta: mi parecer es que actúan igual. Ambos tendrán una 
# columna y y z; y una tercera columna para mean(x).
# Sin embargo parece que no son iguales las salidas porque
df # produce:
# A tibble: 5 × 3
#      x y     z    
#  <int> <chr> <chr>
#1     1 a     K    
#2     2 b     K    
#3     3 a     L    
#4     4 a     L    
#5     5 b     K 

df |>
  group_by(y, z) # produce:
# A tibble: 5 × 3
# Groups:   y, z [3]
#      x y     z    
#  <int> <chr> <chr>
#1     1 a     K    
#2     2 b     K    
#3     3 a     L    
#4     4 a     L    
#5     5 b     K  

# No entiendo bien por qué en el un caso hay 2 grupos y en el otro
# hay 3 grupos. Es un poco extraño, no entiendo bien.

# Genera performance que es el rendimiento que se obtiene del índice
# de hits frente al número de veces que intenta poner la pelota en 
# juego. Donde H es un hit y AB el número de veces
batters <- Lahman::Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
#View(batters)
batters # produce:
# A tibble: 20,985 × 3
# playerID  performance     n
# <chr>           <dbl> <int>
#1 aardsda01      0          4
#2 aaronha01      0.305  12364
#3 aaronto01      0.229    944
#4 aasedo01       0          5

#View(Lahman::Batting)
# Vemos que xiste una correlación positiva entre la habilidad 
# ( performance) y las oportunidades de golpear la pelota ( n) 
# porque los equipos quieren darles a sus mejores bateadores la 
# mayor cantidad de oportunidades de golpear la pelota.
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)
#  Los jugadores con los mejores promedios de bateo son claramente 
# los que intentaron poner la pelota en juego muy pocas veces y 
# lograron un hit; no son necesariamente los jugadores más hábiles:
batters |> 
  arrange(desc(performance)) # produce:
# A tibble: 20,985 × 3
#   playerID  performance     n
#   <chr>           <dbl> <int>
#1 abramge01           1     1
#2 alberan01           1     1
#3 banisje01           1     1
#4 bartocl01           1     1
# Parece que los n atípicos y muy bajos crean distorsiones