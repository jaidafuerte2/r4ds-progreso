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



datetime <- ymd_hms("2026-07-08 12:34:56")
datetime # produce: [1] "2026-07-08 12:34:56 UTC"
year(datetime) # produce:
#> [1] 2026
month(datetime) # produce:
#> [1] 7
# día del mes
mday(datetime) # produce:
#> [1] 8
#
# día del año
yday(datetime) # produce
#> [1] 189
# día de la semana "week day"
wday(datetime)
#> [1] 4

month(datetime, label = TRUE) # produce:
#> [1] Jul
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec
# 
wday(datetime, label = TRUE) # produce:
#[1] Wed
#Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
wday(datetime, label = TRUE, abbr = FALSE)
#> [1] Wednesday
#> 7 Levels: Sunday < Monday < Tuesday < Wednesday < Thursday < ... < Saturday
month(datetime, label = TRUE, abbr = FALSE) # produce:
#[1] July
#12 Levels: January < February < March < April < May < June < ... < December

# Hay más vuelos el fin de semana que entre semana
flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()

# Se retrasan menos los vuelos que salen en los minutos 20-30 y 
# 50-60 que los que salen el resto de la hora
flights_dt |> 
  mutate(minute = minute(dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()
#
minute(530) # produce: 8
minute(517)
flights_dt |> 
  mutate(minute = minute(dep_time)) |> # notar que flights_dt tiene
  group_by(minute) |> # produce:       # dep_time en formato de fecha
  select(minute)
# A tibble: 328,063 × 10
# Groups:   minute [60]
#  origin dest  dep_delay arr_delay dep_time           
#  <chr>  <chr>     <dbl>     <dbl> <dttm>             
#1 EWR    IAH           2        11 2013-01-01 05:17:00
#2 LGA    IAH           4        20 2013-01-01 05:33:00
#3 JFK    MIA           2        33 2013-01-01 05:42:00
#4 JFK    BQN          -1       -18 2013-01-01 05:44:00

# En las salidas programadas no hay un patrón tan marcado
sched_dep <- flights_dt |> 
  mutate(minute = minute(sched_dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
#
ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
  geom_line()

# Graficar los vuelos por semana , segpun su hora de salida
flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(x = week, y = n)) +
  geom_line() + 
  geom_point()

# Contar vuelos por semanas (53-54 semanas). Parece que la función 
# floor_date() recibe una cadena de argumento dependiendo de 
# como se quieran agrupar las fechas. En este caso "week"
flights_dt |> 
  count(week = floor_date(dep_time, "week")) # produce:
# A tibble: 53 × 2
#  week                    n
#  <dttm>              <int>
#1 2012-12-30 00:00:00  4300
#2 2013-01-06 00:00:00  6082
#3 2013-01-13 00:00:00  5976
#4 2013-01-20 00:00:00  5925
#5 2013-01-27 00:00:00  5774

# Utilizar redondeo (floor_date) para mostrar la distribución de 
# vuelos a lo largo del día, calculando la diferencia entre el
# momento actual dep_time y el primer instante de ese día
flights_dt |> 
  mutate(floor_hour = (floor_date(dep_time, "day")),
         dep_hour = dep_time - floor_hour) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

# Mostrar dep_hour que es una resta de la hora de salida menos 
# el inicio del día (que se calcula usando floor date)
flights_dt |> 
  mutate(floor_hour = (floor_date(dep_time, "day")),
         dep_hour = dep_time - floor_hour) |>
  select(dep_time, dep_hour, floor_hour) # produce:
# A tibble: 328,063 × 3
#  dep_time            dep_hour   floor_hour         
#  <dttm>              <drtn>     <dttm>             
#1 2013-01-01 05:17:00 19020 secs 2013-01-01 00:00:00
#2 2013-01-01 05:33:00 19980 secs 2013-01-01 00:00:00
#3 2013-01-01 05:42:00 20520 secs 2013-01-01 00:00:00
#4 2013-01-01 05:44:00 20640 secs 2013-01-01 00:00:00
  
# Usar hms::as_hms para visualizar mejor las horas en el eje X del
# gráfico. Parece que la mayor parte de salidas son entre las 8
# de la mañana y las 3 de la tarde
flights_dt |> 
  mutate(floor_hour = (floor_date(dep_time, "day")),
         dep_hour = hms::as_hms(dep_time - floor_hour)) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

# Es posible usar las funciones de acceso para modificar los 
# componentes de una fecha y hora, para limpiar datos con fechas
# claramente incorrectas
(datetime <- ymd_hms("2026-07-08 12:34:56")) # produce:
#> [1] "2026-07-08 12:34:56 UTC"
#
year(datetime) <- 2030 
datetime # produce:
#> [1] "2030-07-08 12:34:56 UTC"
month(datetime) <- 01
datetime
#> [1] "2030-01-08 12:34:56 UTC"
hour(datetime) <- hour(datetime) + 1
datetime
#> [1] "2030-01-08 13:34:56 UTC"

# También es posible establecer varios valores en un sólo paso
update(datetime, year = 2030, month = 2, mday = 2, hour = 2)
# produce:
#> [1] "2030-02-02 02:34:56 UTC"

# Los valores demasiado grandes se desbordarán:
update(ymd("2023-02-01"), mday = 30) # produce:
#> [1] "2023-03-02" # Se desborda al siguiente mes
update(ymd("2023-02-01"), hour = 400) # produce
#> [1] "2023-02-17 16:00:00 UTC" # Se desborda 16 días

########################
###
### 17.3.4 Ejercicios
###
########################

# NO REALIZADOS

# En R, cuando restas dos fechas, obtienes un objeto difftime:
# How old is Hadley?
h_age <- today() - ymd("1979-10-14")
h_age # produce:
#> Time difference of 16840 days

# Esta salida es un poco ambigua, así que se puede usar las funciones
# de lubridate para ser precisos y explícitos:
as.duration(h_age) # esta función muestra la salida en segundos 
# y años:
#> [1] "1454976000s (~46.11 years)"

# Las duraciones incluyen una serie de funciones prácticas:
dseconds(15) # produce:
#> [1] "15s"
dminutes(10) # produce:
#> [1] "600s (~10 minutes)"
dhours(c(12, 24)) # produce:
#> [1] "43200s (~12 hours)" "86400s (~1 days)"
ddays(0:5) # produce:
#> [1] "0s"                "86400s (~1 days)"  "172800s (~2 days)"
#> [4] "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
dweeks(3) # produce:
#> [1] "1814400s (~3 weeks)"
dyears(1) # produce:
#> [1] "31557600s (~1 years)"
# NOTA: el nombre de las funciones hace referencia a cómo debe
# interpretar, la función, su argumento numérico.

# Es posible sumar y mucltiplicar duraciones
2 * dyears(1) # produce:
#> [1] "63115200s (~2 years)"
dyears(1) + dweeks(12) + dhours(15) # produce:
#> [1] "38869200s (~1.23 years)"

# Es posible sumar y restar duraciones a los días y viceversa:
tomorrow <- today() + ddays(1)
tomorrow # produce: [1] "2025-11-23"
last_year <- today() - dyears(1)
last_year # produce: [1] "2024-11-21 18:00:00 UTC"

# Sin embargo, a veces puede haber resultados inesperados:
one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
#
one_am
#> [1] "2026-03-08 01:00:00 EST"
one_am + ddays(1)
#> [1] "2026-03-09 02:00:00 EDT"
# NOTA: Se suma un día y además una hora (lo que es inesperado),
# además cambió la zona horaria. Esto pasa porque R trabaja con 
# segundos, además el 8 de marzo tiene sólo 23 horas (sorprendente!)
#  así que si añadimos los segundos de un día completo,  obtenemos
# una hora diferente

# Lubridate proporciona períodos que son intervalos de tiempo
# sin una duración fija  en segundos: en cambio funcionan con unidades
# de tiempo "humanas" como días y meses, lo que es más intuitivo
one_am # produce:
#> [1] "2026-03-08 01:00:00 EST"
one_am + days(1) # produce:
#> [1] "2026-03-09 01:00:00 EDT"

# Los períodos también se pueden crear con una serie de funciones 
# constructoras sencillas
hours(c(12, 24)) # produce:
#> [1] "12H 0M 0S" "24H 0M 0S"
days(7)
#> [1] "7d 0H 0M 0S"
months(1:6)
#> [1] "1m 0d 0H 0M 0S" "2m 0d 0H 0M 0S" "3m 0d 0H 0M 0S" "4m 0d 0H 0M 0S"
#> [5] "5m 0d 0H 0M 0S" "6m 0d 0H 0M 0S"

# Es posible sumar y multiplicar periódos 
10 * (months(6) + days(1)) # produce:
#> [1] "60m 10d 0H 0M 0S"
days(50) + hours(25) + minutes(2) # produce:
#> [1] "50d 25H 2M 0S"

# Es posible añadir periodos a las fechas (y es más probable 
# predecir su funcionamiento)
# A leap year
ymd("2024-01-01") + dyears(1) # produce:
#> [1] "2024-12-31 06:00:00 UTC"
ymd("2024-01-01") + years(1) # produce:
#> [1] "2025-01-01" # Esto es lo esperado
#
one_am # produce: [1] "2026-03-08 01:00:00 EST"
# Daylight saving time
one_am + ddays(1)
#> [1] "2026-03-09 02:00:00 EDT"
one_am + days(1)
#> [1] "2026-03-09 01:00:00 EDT" # Esto es lo eserado

