#####################################
###                               ###
###         Capítulo 12           ###
###      Vectores lógicos         ###
###                               ###
#####################################

library(tidyverse)
library(nycflights13)

# Mostrar en una pestaña nueva la tabla flights de nycflights13 
#View(flights)

x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2 # produce:
#[1]  2  4  6 10 14 22 26

df <- tibble(x)
df # produce:
#A tibble: 7 × 1
#      x
#    <dbl>
#1     1
#2     2
#3     3
#4     5

# Filtrar los vuelos que partieron después de las 6, antes de las 20
# y que tuvieron un retraso de llegada menor a 20 minutos
flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

# Crea nuevas columnas daytime y approx_ontime con datos booleanos 
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used" # controla qué columnas quedan en el resultado
    # en este caso quedan sólo las columnas usadas para el cálculo
    # y las nuevas porque se usó el argumento "used"
  ) # produce:
# A tibble: 336,776 × 4
#  dep_time arr_delay daytime approx_ontime
#     <int>     <dbl> <lgl>   <lgl>        
#1      517        11 FALSE   TRUE         
#2      533        20 FALSE   FALSE        
#3      542        33 FALSE   FALSE        
#4      544       -18 FALSE   TRUE   

# Crea nuevas columnas daytime y approx_ontime con datos booleanos
# Es parecido al anterior código
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime) # produce:
# A tibble: 172,286 × 21
#year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      601            600         1      844
#2  2013     1     1      602            610        -8      812
#3  2013     1     1      602            605        -3      821
#4  2013     1     1      606            610        -4      858

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x # produce: [1] 1 2
x == c(1, 2) # produce: FALSE
# NOTA: este resultado se produce porque los cálculos resultan en 
# aproximaciones que no son exactamente  iguales a los número 
# enteros en sí mismo. Por ejemplo:
print(x, digits = 16) # produce:
# [1] 0.9999999999999999 2.0000000000000004
# Es decir r redondea automaticamente estos números
# Qué hacer para comparar cantidades con deciamles tan pequeños?
# Usar la función near de dplyr
dplyr::near(x, c(1, 2)) # produce: [1] TRUE TRUE

# Las operaciones que involucran un valor desconocido, también serán 
# desconocidas
NA > 5 #  produce: NA
10 == NA #  produce: NA


# Añadiendo un poco de contexto, se entiende bien que las operaciones
# que incluyen valores faltantes, devuelven valores faltantes, ejemplo:
# Edad de mary desconocida
age_mary <- NA
# Edad de John desconocida
age_john <- NA
age_mary == age_john

# Este ejemplo falla (no filtra las filas con valores faltantes)
# porque dep_time == NA no devuelve un valor booleano TRUE o FALSE
# sino un valor NA, entonces no filtra nada
flights |> 
  filter(dep_time == NA) # produce:
# A tibble: 0 × 19
# ℹ 19 variables: year <int>, month <int>, day <int>, dep_time <int>,
#   sched_dep_time <int>, dep_delay <dbl>, arr_time <int>,
#   sched_arr_time <int>, arr_delay <dbl>, carrier <chr>, flight <int>,

# Para filtrar valores faltantes es mejor usar la función is.na()
is.na(c(TRUE, NA, FALSE)) # produce:
#[1] FALSE  TRUE FALSE
is.na(c(1, NA, 3)) # produce:
# [1] FALSE  TRUE FALSE
is.na(c("a", NA, "b")) # produce:
[1] FALSE  TRUE FALSE

# Entonces para encontrar filas con valores faltantes en la columna
# dep_time. el código sería el siguiente:
flights |> 
  filter(is.na(dep_time)) # produce:
# A tibble: 8,255 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA

# En este caso , con arrange(), los valores faltantes de la columna
# dep_time, van al final
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(dep_time)
# A tibble: 842 × 19
#year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
#
# Se filtra además por valores faltantes en dep_time, se ven cuatro
flights |> 
  filter(month == 1, day == 1, is.na(dep_time)) |> 
  arrange(dep_time) # produce:
# A tibble: 4 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA
# Se ordena en orden descendente anteponiendo los 4 valores faltantes
flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time) # produce:
# A tibble: 842 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA
#5  2013     1     1      517            515         2      830
#6  2013     1     1      533            529         4      850

########################
###
### 12.2.4 Ejercicios
###
########################

# 1. ¿Cómo dplyr::near()funciona? Escribe nearpara ver el 
# código fuente. ¿Está sqrt(2)^2cerca del 2?
sqrt(2)^2 # produce: 2
sqrt(2)^2 == 2 # produce: FALSE
dplyr::near(sqrt(2)^2, 2) # produce: TRUE

# 2. Utilice mutate(), is.na(), y count()juntos para describir 
# cómo se conectan los valores faltantes en dep_time, 
# sched_dep_timey .dep_delay
flights |> 
  mutate(
    dep_time_na = is.na(dep_time)
  ) |>
  count(dep_time_na) # produce:
# A tibble: 2 × 2
#dep_time_na      n
#  <lgl>        <int>
#1 FALSE       328521
#2 TRUE          8255
#
flights_na_dep_time <- flights |> 
  mutate(
    dep_time_na = is.na(dep_time)
  )
flights_na_dep_time |>
  count(dep_time_na) # produce:
# A tibble: 2 × 2
#  dep_time_na      n
#  <lgl>        <int>
#1 FALSE       328521
#2 TRUE          8255
#
flights_na_sched_dep_time <- flights |> 
  mutate(
    sched_dep_time_na = is.na(sched_dep_time)
  )
flights_na_sched_dep_time |>
  count(sched_dep_time_na) # produce:
# A tibble: 1 × 2
#  sched_dep_time_na      n
#  <lgl>              <int>
#1 FALSE             336776
#
flights_na_dep_delay <- flights |> 
  mutate(
    dep_delay_na = is.na(dep_delay)
  )
flights_na_dep_delay |>
  count(dep_delay_na) # produce:
# A tibble: 2 × 2
#dep_delay_na      n
#  <lgl>         <int>
#1 FALSE        328521
#2 TRUE           8255
#
# RESPUESTA : MI parecer es que el retraso en la partida se relaciona
# o se calcula a partir de la hora de salida. En cambio la hora 
# programada de salida no se relaciona con la hora de salida (real).
# Por eso la hora de salida y el retraso en la hora de salida
# tienen 8255 valores faltantes. Sin embargo el retraso de la hora de
# salida también se calcula a apartir de la hora programada de 
# salida. Pero esto no afecta el número de valores faltantes en
# el retraso de la hora de salida porque el horario programado de
# salida no tiene valores faltantes. Lo que puede estar pasando es
# que un vuelo programado, simplemente, no se realizó.

df <- tibble(x = c(TRUE, FALSE, NA))
# El resultado de esta mutación recuuerda los resultados de lisp de
# las operaciones booleanas, tomando en cuenta que NA se considera
# un valor TRUE no FALSE. Entonces and devuelve el primer valor falso
# o el útimo verdadero. Y or devuelve el primer valor verdadero o el
# último falso
df |> 
  mutate(
    and = x & NA,
    or = x | NA
  )
# A tibble: 3 × 3
# x     and   or   
# <lgl> <lgl> <lgl>
#1 TRUE  NA    TRUE 
#2 FALSE FALSE NA   
#3 NA    NA    NA   
# De esta forma
TRUE & NA # produce: NA
FALSE & NA # produce: FALSE
NA & NA # produce: NA
TRUE | NA # produce: TRUE
FALSE | NA # produce: NA
NA | NA # produce: NA
NA & TRUE # produce: NA # Sin embargo esto falla con la lógica de 
# que NA es un valor # verdadero. En realidad NA es TRUE o FALSE 
# así que cuando va primero 
# en una operación booleana, el resultado siempre será NA.

# Seleccionar las filas de vuelos de noviembre y diciembre
flights |>
  filter(month == 11 | month == 12) # produce:
# A tibble: 55,403 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013    11     1        5           2359         6      352
#2  2013    11     1       35           2250       105      123
#3  2013    11     1      455            500        -5      641
#4  2013    11     1      539            545        -6      856
#
# Esta versión es incorrecta para el propósito de seleccionar los 
# vuelos de Diciembre pero todavía funciona
flights |> 
  filter(month == 11 | 12) # produce: 
# A tibble: 336,776 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004
# Qué pasó? Pasó que cuando un número está dentro de una expresión
# booleana, todo número distinto de cero es TRUE (lo clásico):
TRUE | 12 # produce: TRUE
12 | FALSE # produce: TRUE
# Entonces: 
flights |> 
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used" # para mantener en la tabla las nuevas variables
    # y las variables antiguas que se usaron en mutate
  ) # produce:
# A tibble: 336,776 × 3
#  month nov   final
#  <int> <lgl> <lgl>
#1     1 FALSE TRUE 
#2     1 FALSE TRUE 
#3     1 FALSE TRUE 
#4     1 FALSE TRUE 

1:12 # produce:  [1]  1  2  3  4  5  6  7  8  9 10 11 12
1:12 %in% c(1, 5, 11) # produce:
# [1]  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
#[12] FALSE
# Encontrar todos los vuelos de noviembre y diciembre
flights |>
  filter(month %in% c(11, 12)) # produce:
# A tibble: 55,403 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013    11     1        5           2359         6      352
#2  2013    11     1       35           2250       105      123
#3  2013    11     1      455            500        -5      641
#4  2013    11     1      539            545        -6      856

# == e %in% siguen reglas diferentes para NA
NA == NA # produce: NA
NA %in% NA # produce: TRUE
c(1, 2, NA) == NA # produce: [1] NA NA NA
c(1, 2, NA) %in% NA # produce: [1] FALSE FALSE  TRUE

flights |> 
  filter(dep_time %in% c(NA, 0800)) # produce:
# A tibble: 8,803 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      800            800         0     1022
#2  2013     1     1      800            810       -10      949
#3  2013     1     1       NA           1630        NA       NA
#4  2013     1     1       NA           1935        NA       NA

########################
###
### 12.3.4 Ejercicios
###
########################

# 1. Find all flights where arr_delay is missing but dep_delay 
# is not. Find all flights where neither arr_time nor 
# sched_arr_time are missing, but arr_delay is.

flights |>
  filter(is.na(arr_delay) & is.na(dep_delay)) # produce:
# A tibble: 8,255 × 19
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1       NA           1630        NA       NA
#2  2013     1     1       NA           1935        NA       NA
#3  2013     1     1       NA           1500        NA       NA
#4  2013     1     1       NA            600        NA       NA

# 2. ¿Cuántos vuelos tienen un valor faltante dep_time? ¿Qué otras 
# variables faltan en estas filas? ¿Qué podrían representar estas 
# filas?

flights |>
  filter(is.na(dep_time)) |>
  count(dep_time) # produce:
# A tibble: 1 × 2
#dep_time     n
#     <int> <int>
#1       NA  8255
# Hay 8255 que no tienen valores en la variable dep_time
flights_dep_time_na <- flights |>
  filter(is.na(dep_time)) 
#View(flights_dep_time_na)
# Si una observación o fila no tiene dep_time, tampoco tiene dep_delay
# porque un vuelo que no sale no puede retrasarse, tampoco hay
# arrival_time porque un vuelo que no sale no llega. Y tampoco hay
# arr_delay porque un vuelo que no sale no llega y si no llega no
# puede tener retraso.
# Sin embargo no todas las observaciones 
flights |> 
  filter(is.na(arr_time) & is.na(sched_arr_time) & is.na(arr_delay))


# 3. Suponiendo que la ausencia de un dato dep_time implica la 
# cancelación de un vuelo, analicemos el número de vuelos 
# cancelados por día. ¿Existe algún patrón? ¿Hay alguna relación 
# entre la proporción de vuelos cancelados y el retraso promedio
# de los vuelos no cancelados?
# Crear una nueva tabla con una nueva variable que se llama 
# cancelled, que tiene TRUE como valor cuando hay NA en la columna
# de la variable dep_time
flights_daily <- flights |>
  mutate(cancelled = is.na(dep_time))
flights_daily # produce:
# A tibble: 336,776 × 20
#   year month   day dep_time sched_dep_time dep_delay arr_time
#  <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1  2013     1     1      517            515         2      830
#2  2013     1     1      533            529         4      850
#3  2013     1     1      542            540         2      923
#4  2013     1     1      544            545        -1     1004

# Crea una nueva tabla agrupada por día del año
daily_summary <- flights_daily |>
  # Agrupar por día de l año
  group_by(year, month, day) |>
  # Crear nuevas variables dentro de la nueva tabla agrupada 
  summarise(
    # Suma los vuelos (obesrvaciones o filas) con TRUE en la columna
    # de la variable cancelled
    n_cancelled = sum(cancelled),
    # Cuenta número de observaciones
    n_flights = n(),
    # Proporción de vuelos cancelados
    prop_cancelled = mean(cancelled),
    # Promedio de restrasos de salida
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )
#View(daily_summary)
# Graficar los vuelos cancelados por días del año
library(ggplot2)
daily_summary |>
  ggplot(aes(x = day + (month - 1) * 31, y = prop_cancelled)) +
  geom_line() +
  labs(
    x = "Día del año (aproximado)",
    y = "Proporción de vuelos cancelados",
    title = "Cancelaciones de vuelos por día en 2013"
  )
# Conocer la relación entre vuelos cancelados y retraso promedio
daily_summary |>
  ggplot(aes(x = avg_delay, y = prop_cancelled)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    x = "Retraso promedio (minutos)",
    y = "Proporción de vuelos cancelados",
    title = "Relación entre retrasos y cancelaciones"
  )


# Agrupar los vuelos por día del año y saber sí todos los vuelos
# tuvieron un retraso de partida <= 60 o si algún vuelo tuvo un 
# retraso de llegada mayor a 300. all() funciona como AND & y any
# funciona con OR | . na.rm = TRUE significa ignora los datos faltantes
# para que no arruinen el resultado . .group = "drop" se usa para
# desagrupar la tabla para cálculos en los que no se necesite agrupar
# en el futuro. Muy importante
flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )
# produce:
# A tibble: 365 × 5
#   year month   day all_delayed any_long_delay
#  <int> <int> <int> <lgl>       <lgl>         
#1  2013     1     1 FALSE       TRUE          
#2  2013     1     2 FALSE       TRUE          
#3  2013     1     3 FALSE       FALSE         
#4  2013     1     4 FALSE       FALSE    
# 
x <- c(TRUE, TRUE, NA)
all(x) # produce:[1] NA
all(x, na.rm = TRUE) # produce: TRUE

# Crea nuevas tablas de tipo numérico es decir la proporción de
# los retrasos menores o iguales a 60 y la suma de los vuelos que
# retrasaron en su llegada más de 300. mean() suma los valores
# TRUE sobre el total de valores que hay 
# en la columna dep_delay. sum() simplemente suma todos los valores
# TRUE que hay en la columna arr_delay
flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  ) # produce:
# A tibble: 365 × 5
#   year month   day proportion_delayed count_long_delay
#  <int> <int> <int>              <dbl>            <int>
#1  2013     1     1              0.939                3
#2  2013     1     2              0.914                3
#3  2013     1     3              0.941                0
#4  2013     1     4              0.953                0

# Filtra los vuelos que tuvieron retraso en su llegada y los agrupa
# por día del año; luego crea una variable behind para sacar los
# promedios de minutos de retraso y n muestra la cantidad de vuelos
# que se retrasaron (lo que se filtró)
flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  ) # produce:
# A tibble: 365 × 5
#   year month   day behind     n
#  <int> <int> <int>  <dbl> <int>
#1  2013     1     1   32.5   461
#2  2013     1     2   32.0   535
#3  2013     1     3   27.7   460
#4  2013     1     4   28.3   297
#
# Este código es distinto porque no filtra, entonces n es igual 
# al número total de vuelos. Además se puede crear una variable
# para vuelos que se retrasaron en la llegada (behind) y otra 
# variable para los que se adelantaron (ahead)
flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) # produce:
# A tibble: 365 × 6
#   year month   day behind ahead     n
#  <int> <int> <int>  <dbl> <dbl> <int>
#1  2013     1     1   32.5 -12.5   842
#2  2013     1     2   32.0 -14.3   943
#3  2013     1     3   27.7 -18.2   914
#4  2013     1     4   28.3 -17.0   915

########################
###
### 12.4.4 Ejercicios
###
########################

# 1. What will sum(is.na(x)) tell you? How about mean(is.na(x))?
x <- c(10, NA, 5, NA, 8)
x # produce: [1] 10 NA  5 NA  8
is.na(x) # produce: [1] FALSE  TRUE FALSE  TRUE FALSE
sum(is.na(x)) # produce: [1] 2 # porque sum suma los valores TRUE
# de una columna
mean(is.na(x)) # produce:  [1] 0.4 # porque mean cuenta los TRUE
# de una columna y los divide por el total de valores o filas
# de esa columna. Es decir 2 / 5 = 0.4

# 2. What does prod() return when applied to a logical vector? 
# What logical summary function is it equivalent to? What does 
# min() return when applied to a logical vector? What logical 
# summary function is it equivalent to? Read the documentation 
# and perform a few experiments.
y <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
prod(y) # produce: 0
?prod() # : returns the product of all the values present 
# in its arguments.
prod(c(1, 2, 3)) # produce: 6
# Es decir prod() devuelve cero 0 cuando tiene valores booleanos
min(y) # produce: 0
?min() # : yo supongo que igual que en otros lenguajes min devuelve el
# número menor
min(c(8, 4, 9, 5, 2)) # produce: 2
# Entonces cuando a min se le da un vector lógico, deveulve cero 0
# 
# Sin embargo el comportamiento cambia cuando todos son TRUE
z <- c(TRUE, TRUE, TRUE)
prod(z) # produce: 1
min(z) # produce: 1
# Esto hace que el comportamiento de min y prod sea igual a ll
all(y) # produce: FALSE
all(z) # produce: TRUE



x <- c(-3:3, NA)
x # produce: [1] -3 -2 -1  0  1  2  3 NA
# Lo común es que una función if_else() tome 3 argumentos;
# el primer argumento es una condición, el segundo argumento es
# el resultado si la condición es TRUE; y el tercer argumento es
# el resultado si la condición es FALSE
if_else(x > 0, "+ve", "-ve") # produce:
#[1] "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" NA  
# 
# La función if_else toma un cuarto argumento opcional que se utilizará
# si el resultado de la condición es NA
if_else(x > 0, "+ve", "-ve", "???") # produce:
#[1] "-ve" "-ve" "-ve" "-ve" "+ve" "+ve" "+ve" "???"
#
# El resultado de la función if_else() puede ser un vector
if_else(x < 0, -x, x) # produce:
#[1]  3  2  1  0  1  2  3 NA

# Con r y la función if_else() es posible combinar dos vectores
x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1) # produce:
#[1] 3 1 2 6

# Al puro estilo de lisp, la función if_else() se puede anidar
if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")
#[1] "-ve" "-ve" "-ve" "0"   "+ve" "+ve" "+ve" "???"

# Para mejorar que se entienda mejor y no necesitar anidar if's, se 
# puede usar case_when 

x <- c(-3:3, NA)
case_when(
  x == 0 ~ "0",
  x < 0 ~ "-ve", 
  x > 0 ~ "+ve",
  is.na(x) ~ "???"
)# produce:
#[1] "-ve" "-ve" "-ve" "0"   "+ve" "+ve" "+ve" "???"
# 
# Si ninguno de los casos coincide, la salida será NA
case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve"
) # produce:
#[1] "-ve" "-ve" "-ve" NA    "+ve" "+ve" "+ve" NA 
#
# Se puede usar .default para tener un valor predeterminado similar
# a PHP
case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
)
# Si se cumplen varias condiciones sólo se utilizará la primera
case_when(
  x > 0 ~ "+ve",
  x > 2 ~ "big"
) # produce:
#[1] NA    NA    NA    NA    "+ve" "+ve" "+ve" NA

# Creo una nueva variable llamada status que categorizará la variable
# numérica arr_delay y depende de la cantidad de retraso de llegada 
# de un vuelo
flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used" # Para mantener en la nueva tabla sólo las variables
    # que utlizo en mutate y la nueva variable que creo en mutate
  ) # Porduce:
# A tibble: 336,776 × 2
#  arr_delay status 
#      <dbl> <chr>  
#1        11 on time
#2        20 late   
#3        33 late   
#4       -18 early  

# Los tipos de la salida de una función condicional deben ser 
# compatibles. Esto probablememte se debe a que un vector es un
# conjunto de datos del mismo tipo (a diferencia de una lista)
if_else(TRUE, "a", 1) # produce:
#Error in `if_else()`:
#! Can't combine `true` <character> and `false` <double>.
#
case_when(
  x < -1 ~ TRUE,  
  x > 0  ~ now()
) # produce:
#Error in `case_when()`:
#! Can't combine `..1 (right)` <logical> and `..2 (right)` 
# <datetime<local>
#
# Qué tipos de datos son compatibles: números y booleanos; cadenas
# y factores (cadenas con un conjunto restringido de valores);
# la fechas y las fechas y horas. NA es compatible con cualquier 
# tipo de dato.

########################
###
### 12.5.4 Ejercicios
###
########################

# 1. A number is even if it’s divisible by two, which in R you can 
# find out with x %% 2 == 0. Use this fact and if_else() to 
# determine whether each number between 0 and 20 is even or odd.
x <- c(0:20)
x
if_else(x %% 2 == 0, "even", "odd") # produce:
#[1] "odd"  "even" "odd"  "even" "odd"  "even" "odd"

# 2. Given a vector of days like x <- c("Monday", "Saturday", 
# "Wednesday"), use an if_else() statement to label them as 
# weekends or weekdays.
x <- c("Monday", "Saturday", "Wednesday")
if_else(x == "Sunday" | x == "Saturday", "Weekend", "Weekday")
# produce:
#[1] "Weekday" "Weekend" "Weekday"

# 3. Use if_else() to compute the absolute value of a numeric 
# vector called x.
x <- c(-3:3)
x # produce:
#[1] -3 -2 -1  0  1  2  3
if_else(x < 0, -x, x) # produce:
#[1] 3 2 1 0 1 2 3

# 4. Write a case_when() statement that uses the month and day 
# columns from flights to label a selection of important US 
# holidays (e.g., New Years Day, 4th of July, Thanksgiving, and 
# Christmas). First create a logical column that is either TRUE or 
# FALSE, and then create a character column that either gives the 
# name of the holiday or is NA.
flights |> 
  mutate(
    holidays_lgl = case_when(
      month == 7 & day == 4 ~ TRUE,
      month == 1 & day == 1 ~ TRUE,
      month == 12 & day == 24 ~ TRUE,
      .default = FALSE
    ),
    holidays_str = case_when(
      month == 7 & day == 4 ~ "4 de Julio",
      month == 1 & day == 1 ~ "Thanksgiving",
      month == 12 & day == 24 ~ "Christmas"
    ),
    .keep = "used"
  )



