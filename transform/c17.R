#####################################
###                               ###
###         Capítulo 17           ###
###            Fechas             ###
###           y horas             ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

# Mostrar la fecha actual:
today() # produce:
[1] "2025-11-21"
# Mostrar la hora actual
now() # produce:
[1] "2025-11-21 13:58:41 UTC"

# Si un archivo CSV viene con fechas y horas en formato ISO8601 no
# se necesita hacer nada porque readr reconoce este formato 
# automáticamente.
csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv) # produce:
#> # A tibble: 1 × 2
#>   date       datetime           
#>   <date>     <dttm>             
#> 1 2022-01-02 2022-01-02 05:12:00

# Para otros formatos se podría usar col_date()
csv <- "
  date
  01/02/15
"
# Mostrar la tabla de fechas en formato iso
read_csv(csv, col_types = cols(date = col_date("%m/%d/%y"))) 
# produce:
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-01-02
# Mostrar la tabla de fechas en formato iso
read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
# produce:
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-02-01
# Mostrar la tabla de fechas en formato iso
read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))
# produce:
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2001-02-15

# lubridate tiene funciones para analizar y transformar los formatos
# de fecha que vienen en forma de cadenas
ymd("2017-01-31") # produce:
#> [1] "2017-01-31"
mdy("January 31st, 2017") # produce:
#> [1] "2017-01-31"
dmy("31-Jan-2017") # produce:
#> [1] "2017-01-31"

# Para analizar y transformar fechas y horas, lubridate tiene otras
# funciones, de nombres intuitivos
ymd_hms("2017-01-31 20:11:59") # produce:
#> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01") # produce:
#> [1] "2017-01-31 08:01:00 UTC"

# Forzar la creación de fecha y hora a partir de una zona horaria:
ymd("2017-01-31", tz = "UTC") # produce:
#> [1] "2017-01-31 UTC"


flights <- nycflights13::flights
# A veces los componentes individuales de fechas y horas, vienen 
# distribuidos en distintas columnas
nycflights13::flights |> 
  select(year, month, day, hour, minute)
#> # A tibble: 336,776 × 5
#>    year month   day  hour minute
#>   <int> <int> <int> <dbl>  <dbl>
#> 1  2013     1     1     5     15
#> 2  2013     1     1     5     29
#> 3  2013     1     1     5     40
#> 4  2013     1     1     5     45
#> 5  2013     1     1     6      0
#> 6  2013     1     1     5     58
#> # ℹ 336,770 more rows

# Para crear una fecha y hora a partir de este tipo de entrada, se
# puede utilizar make_date() o make_datetime()
flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute))
# produce:
#> # A tibble: 336,776 × 6
#>    year month   day  hour minute departure          
#>   <int> <int> <int> <dbl>  <dbl> <dttm>             
#> 1  2013     1     1     5     15 2013-01-01 05:15:00
#> 2  2013     1     1     5     29 2013-01-01 05:29:00
#> 3  2013     1     1     5     40 2013-01-01 05:40:00
#> 4  2013     1     1     5     45 2013-01-01 05:45:00
#> 5  2013     1     1     6      0 2013-01-01 06:00:00
#> 6  2013     1     1     5     58 2013-01-01 05:58:00
#> # ℹ 336,770 more rows

View(flights)
# make_datetime_100() : num num num num -> dttm
# Crear una fecha y hora a partir de las variables year, month, day
# y time gracias a la función make_date_time
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
# Redefinir variables para que tengan formato de fecha/hora
flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))
#
flights_dt # produce:
#> # A tibble: 328,063 × 9
#>   origin dest  dep_delay arr_delay dep_time            sched_dep_time     
#>   <chr>  <chr>     <dbl>     <dbl> <dttm>              <dttm>             
#> 1 EWR    IAH           2        11 2013-01-01 05:17:00 2013-01-01 05:15:00
#> 2 LGA    IAH           4        20 2013-01-01 05:33:00 2013-01-01 05:29:00
#> 3 JFK    MIA           2        33 2013-01-01 05:42:00 2013-01-01 05:40:00
#> 4 JFK    BQN          -1       -18 2013-01-01 05:44:00 2013-01-01 05:45:00
#> 5 LGA    ATL          -6       -25 2013-01-01 05:54:00 2013-01-01 06:00:00
#> 6 EWR    ORD          -4        12 2013-01-01 05:54:00 2013-01-01 05:58:00
#> # ℹ 328,057 more rows
#> # ℹ 3 more variables: arr_time <dttm>, sched_arr_time <dttm>, …

# Visualizar la distribución de los horarios de salida a lo largo
# del año por segundo del día
flights_dt |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day
#
# Visualizar la distribución de los horarios de salida de un sólo
# día a lo largo del día
flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

today() # produce: [1] "2025-11-21"
# Altenar entre fecha y fecha/hora:
as_datetime(today()) # produce:
#> [1] "2025-11-20 UTC"
now() # produce: [1] "2025-11-21 14:59:40 UTC"
# Alternar entre fecha/hora y fecha
as_date(now()) # produce:
#> [1] "2025-11-20"

########################
###
### 16.2.5 Ejercicios
###
########################

# NO REALIZADOS

