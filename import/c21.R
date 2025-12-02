#####################################
###                               ###
###         Capítulo 21           ###
###          Databases            ###
###                               ###
#####################################

library(DBI)
library(dbplyr)
library(tidyverse)
library(duckdb)

# Para conectar r a una base de datos debo usar la función 
# DBI::dbConnect(). El primer argumento selecciona el  sistema de 
# gestión de contenido y el segundo argumento y los siguientes,
# describen como conectarse, es decir donde se encuentran las credenciales 
# necesarias para acceder:
#con <- DBI::dbConnect(
#  RMariaDB::MariaDB(), 
#  username = "foo"
#)
#con <- DBI::dbConnect(
#  RPostgres::Postgres(), 
#  hostname = "databases.mycompany.com", 
#  port = 1234
#)
# NOTA : Los detalles precisos de conexión varían mucho de un sistema 
# de gestión de base de datos a otro.

# Para los ejemplos de este capítulo se usa duckdb.
# Conectarse a una base de datos duckdb
con <- DBI::dbConnect(duckdb::duckdb())
# Conectarse a una base de datos duckdb y especificar dónde guardar
# el archivo persistente con la base de datos. En este caso sería
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

# Para cargar algunos datos se necesita: el primer argumento es una 
# conexión a la base de datos, el nombre de la tabla que se creará
# en la base de datos y un marco de datos con los datos
dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

# Enumerar todas las tablas de la base de datos
dbListTables(con)
#> [1] "diamonds" "mpg"
# Recuperar el contenido de una tabla
con |> 
  dbReadTable("diamonds") |> 
  as_tibble() # Esta función se usa para que la tabla se imprima en un 
              # formato adecuado
# produce:
#> # A tibble: 53,940 × 10
#>   carat cut       color clarity depth table price     x     y     z
#>   <dbl> <fct>     <fct> <fct>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#> 1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
#> 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
#> 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
#> 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
#> 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
#> 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
#> # ℹ 53,934 more rows

# Guardar una consulta a la base de datos
sql <- "
  SELECT carat, cut, clarity, color, price 
  FROM diamonds 
  WHERE price > 15000
"
# Mostrar los resultados de una consulta a la base de datos
as_tibble(dbGetQuery(con, sql)) # produce:
#> # A tibble: 1,655 × 5
#>   carat cut       clarity color price
#>   <dbl> <fct>     <fct>   <fct> <int>
#> 1  1.54 Premium   VS2     E     15002
#> 2  1.19 Ideal     VVS1    F     15005
#> 3  2.1  Premium   SI1     I     15007
#> 4  1.69 Ideal     SI1     D     15011
#> 5  1.5  Very Good VVS2    G     15013
#> 6  1.73 Very Good VS1     G     15014
#> # ℹ 1,649 more rows

# Parece que no se puede usar directamente estos datos que se cargaron
# para usarlos se debe usar dbplyr() lo que conlleva unos pasos 
# adicionales.

# Lo primero es que hay que usar tbl para crear un objeto que represente
# una tabla de base de datos:
diamonds_db <- tbl(con, "diamonds")
diamonds_db # produce:
#> # Source:   table<diamonds> [?? x 10]
#> # Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>   carat cut       color clarity depth table price     x     y     z
#>   <dbl> <fct>     <fct> <fct>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#> 1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
#> 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
#> 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
#> 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
#> 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
#> 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48
#> # ℹ more rows

# Filtrar y seleccionar de diamonds_db , crea un objeto Perezoso que
# solo se ejecuta por completo cuando se necesita:
big_diamonds_db <- diamonds_db |> 
  filter(price > 15000) |> 
  select(carat:clarity, price)
# Llamar a la nueva tabla
big_diamonds_db
#> # Source:   SQL [?? x 5]
#> # Database: DuckDB 1.4.2 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>   carat cut       color clarity price
#>   <dbl> <fct>     <fct> <fct>   <int>
#> 1  1.54 Premium   E     VS2     15002
#> 2  1.19 Ideal     F     VVS1    15005
#> 3  2.1  Premium   I     SI1     15007
#> 4  1.69 Ideal     D     SI1     15011
#> 5  1.5  Very Good G     VVS2    15013
#> 6  1.73 Very Good G     VS1     15014
#> # ℹ more rows
# NOTA: No  se llega a conocer el número de filas porque para eso
# se debería ejecutar la tabla entera y no se quiere eso porque no
# es eficiente. Además se puede saber que este objeto representa una
# conexión a una base de datos porque imprime el nombre de un 
# sistema de gestión de base de datos en la parte superior

# Lo que se está haciendo por adentro es traducir el código r a sql.
# Para ver el comando sql que se está usando para la consulta, se 
# puede usar la función show_query():
big_diamonds_db |>
  show_query()
#> <SQL>
#> SELECT carat, cut, color, clarity, price
#> FROM diamonds
#> WHERE (price > 15000.0)

# Para recuperar los datos en R se puede usar la función collect(),
# lo que convierte la tabla sql a un tibble
big_diamonds <- big_diamonds_db |> 
  collect()
big_diamonds # produce:
#> # A tibble: 1,655 × 5
#>   carat cut       color clarity price
#>   <dbl> <fct>     <fct> <fct>   <int>
#> 1  1.54 Premium   E     VS2     15002
#> 2  1.19 Ideal     F     VVS1    15005
#> 3  2.1  Premium   I     SI1     15007
#> 4  1.69 Ideal     D     SI1     15011
#> 5  1.5  Very Good G     VVS2    15013
#> 6  1.73 Very Good G     VS1     15014
#> # ℹ 1,649 more rows

# Incorporar flights y planes a la base de datos
dbplyr::copy_nycflights13(con) # produce:
#> Creating table: airlines
#> Creating table: airports
#> Creating table: flights
#> Creating table: planes
#> Creating table: weather
#
# Lo primero es que hay que usar tbl para crear un objeto que represente
# una tabla de base de datos:
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

# En sql hay 3 sentencias comunes: CREATE, INSERT Y SELECT. Pero puede 
# ser que para este contexto sea importante sobre todo SELECT porque
# es la sentencia con la que se hacen consultas. En ciencia de datos
# prodbablemente no se necesite usar CREATE ni INSERT.
# También hay 5 cláusulas importantes en sql: SELECT, FROM, WHERE,
# ORDER BY y GROUP BY. Así las cosas, cada consulta debe tener
# SELECT y FROM y la consulta más simple es SELECT * FROM table , lo
# que permite seleccionar todas las columnas de una tabla.
# Así se mostraría en SQL una consulta traducida con la función
# show_query():
flights |> show_query()
#> <SQL>
#> SELECT *
#> FROM flights
planes |> show_query()
#> <SQL>
#> SELECT *
#> FROM planes

# WHERE controla que filas se incluyen y ORDER BY como se ordenan
flights |> 
  filter(dest == "IAH") |> 
  arrange(dep_delay) |>
  show_query() # produce:
#> <SQL>
#> SELECT flights.*
#> FROM flights
#> WHERE (dest = 'IAH')
#> ORDER BY dep_delay

# GROUP BY convierte la consulta en un resumen, lo que provoca que 
# se produzca la agregación
flights |> 
  group_by(dest) |> 
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  show_query()
#> <SQL>
#> SELECT dest, AVG(dep_delay) AS dep_delay
#> FROM flights
#> GROUP BY dest

# SELECT sql es similar select de r
planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  show_query() # produce:
#> <SQL>
#> SELECT tailnum, "type", manufacturer, model, "year" # Usan comillas
#> # porque son palabras reservadas
#> FROM planes
#
# SELECT de sql  es similar a rename de R
planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  rename(year_built = year) |> 
  show_query() # produce:
#> <SQL>
#> SELECT tailnum, "type", manufacturer, model, "year" AS year_built
#> FROM planes
#
# SELECT de sql es similar a relocate de R
planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  relocate(manufacturer, model, .before = type) |> 
  show_query() # produce:
#> <SQL>
#> SELECT tailnum, manufacturer, model, "type", "year"
#> FROM planes

# SELECT de sql también es similar a mutate() de R
flights |> 
  mutate(
    speed = distance / (air_time / 60)
  ) |> 
  show_query() # produce:
#> <SQL>
#> SELECT flights.*, distance / (air_time / 60.0) AS speed
#> FROM flights

# GROUP BY de sql es similar a broup_by() de R y SELECT de sql 
# es similar a summarize de R
diamonds_db |> 
  group_by(cut) |> 
  summarize(
    n = n(),
    avg_price = mean(price, na.rm = TRUE)
  ) |> 
  show_query() # produce:
#> <SQL>
#> SELECT cut, COUNT(*) AS n, AVG(price) AS avg_price
#> FROM diamonds
#> GROUP BY cut

# WHERE de sql es similar a filter() de R
flights |> 
  filter(dest == "IAH" | dest == "HOU") |> 
  show_query() # produce:
#> <SQL>
#> SELECT flights.*
#> FROM flights
#> WHERE (dest = 'IAH' OR dest = 'HOU')
#
flights |> 
  filter(arr_delay > 0 & arr_delay < 20) |> 
  show_query() # produce:
#> <SQL>
#> SELECT flights.*
#> FROM flights
#> WHERE (arr_delay > 0.0 AND arr_delay < 20.0)

# IN de sql es similar a %in% de R:
flights |> 
  filter(dest %in% c("IAH", "HOU")) |> 
  show_query() # produce:
#> <SQL>
#> SELECT flights.*
#> FROM flights
#> WHERE (dest IN ('IAH', 'HOU'))

# NULL de sql es similar NA de R
flights |> 
  filter(!is.na(dep_delay)) |> 
  show_query() # produce:
#> <SQL>
#> SELECT flights.*
#> FROM flights
#> WHERE (NOT((dep_delay IS NULL)))

# HAVING de sql es similar a filter() de R cuando filter() se usa 
# junto a summarize():
diamonds_db |> 
  group_by(cut) |> 
  summarize(n = n()) |> 
  filter(n > 100) |> 
  show_query() # produce:
#> <SQL>
#> SELECT cut, COUNT(*) AS n
#> FROM diamonds
#> GROUP BY cut
#> HAVING (COUNT(*) > 100.0)