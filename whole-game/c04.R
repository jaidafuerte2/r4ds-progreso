#####################################
###                               ###
###         Capítulo 4            ###
###      Flujo de Trabajo:        ###
###      Estilo de código         ###
###                               ###
##################################### 
#install.packages("styler")
# styler:  para acceder a style debo presionar Ctrl+Shift+P y escribir
# styler
library(tidyverse)
library(nycflights13)

# Los nombres de variables (creados por <- y mutate) deben usar sólo 
# letras minúsculas, números y _ para separar palabras dentro de un 
# nombre
# Es correcto escribir así
short_flights <- flights |> filter(air_time < 60) |>
  relocate(air_time)
short_flights # produce:
# A tibble: 52,433 × 19
#   air_time  year month   day dep_time sched_dep_time dep_delay arr_time
#      <dbl> <int> <int> <int>    <int>          <int>     <dbl>    <int>
#1       53  2013     1     1      557            600        -3      709
#2       44  2013     1     1      559            559         0      702
#33       40  2013     1     1      629            630        -1      721
#4       52  2013     1     1      632            608        24      740
# Pero se debe evitar:
SHORTFLIGHTS <- flights |> filter(air_time < 60)
# Por regla general es mejor usar nombres largos pero descriptivos
# que nombres cortos concisos. 
#  Además es mejor usar un prefijo común que un subfijo común

# Se deben poner espacios antes  y después de los operadores matemáticos
# excepto el exponencial ^ . Por ejemplo:
# Se permite:
#z <- (a + b)^2 / d
# Se debe evitar:
#z<- ( a + b ) ^ 2/d

# No se debe dejar espacios dentro ni fuera de los paréntesis en las 
# llamadas a FUNCIONES REGULARES. Siempre se debe dejar una espacio luego 
# de la coma como en  el  inglés estándar. Po ejemplo:
# Esto se permite:
#mean(x, na.rm = TRUE)
#Esto se debe evitar
#mean (x , na.rm=TRUE )

# Esta bien escribir espacios adicionales si esto mejora la alineación
# Por ejemplo si se crean varias variables en  mutate, añadir espacios 
# sería aceptable porque mejora la escritura del código:
flights |> 
  mutate(
    speed      = distance / air_time,
    dep_hour   = dep_time %/% 100,
    dep_minute = dep_time %% 100
  )
# En lo personal no me gusta

# Las tuberías "|>" deben ir precedidas de un espacio y, en general,
# deben ser los últimos elementos de una línea. Por ejemplo:
# Esto está permitido:
flights |>
  filter(!is.na(arr_delay), (!is.na(tailnum))) |>
  count(dest)
# Se debe evitar:
flights |>filter(!is.na(arr_delay), (!is.na(tailnum)))|>count(dest)

# Si una función tiene varios argumentos con nombre (como mutate() 
# o summarize()) cada argumento debe estar en una nueva línea. Si la
# función no tiene argumentos con nombre (como select() o filter()),
# se debe, mantener todo en una sola línea a menos que no quepa.
# En ese caso cada argumento debe estar en su propia línea. Por ejemplo
# Se permite:
flights |> 
  group_by(tailnum) |>
  summarize(
    delay = mean(arr_delay, na.rm = TRUE), 
    n = n()
  )
# Se debe evitar:
flights |> 
  group_by(
    tailnum
    ) |>
  summarize(delay = mean(arr_delay, na.rm = TRUE), n = n())

# La sangría son dos espacios después de un salto de línea tras un 
# |> . Los argumentos de una función, si van en una línea independiente
# también de sangran con dos espacios. Por ejemplo:
# Se permite:
flights |>  
  group_by(tailnum) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
# Se debe evitar sangrar los argumentos a la misma altura del 
# paréntesis:
flights|>
  group_by(tailnum) |> 
  summarize(
            delay = mean(arr_delay, na.rm = TRUE), 
            n = n()
  )
# Se debe evitar sangrar los argumentos a la misma altura de su 
# función:
flights|>
  group_by(tailnum) |> 
  summarize(
  delay = mean(arr_delay, na.rm = TRUE), 
  n = n()
  )

# Si es una expresión corta se podría evitar estas reglas para poner
# toda la expresión en una sóla línea. Por ejemplo:
# Se permite:
df |> mutate(y = x + 1)
# Enlugar de:
df |>
  mutate(
    y = x + 1
  )
# Además se debe evitar crear tuberías demasiado largas de más de 10
# o 15 líneas. Es mejor dividir las tuberías en subtareas pequeñas
# asignando un nombre informativo a cada una.  Siempre que se pueda 
# asignarle a algo un nombre informativo, debe hacerlo, por ejemplo, 
# al cambiar radicalmente la estructura de los datos, por ejemplo, 
# después de pivotar o resumir. ¡No espere acertar a la primera! 
# Esto significa dividir las tuberías largas si existen estados 
# intermedios que puedan tener buenos nombres.

# En el caso de los gráficos, a los símbolos + se les trata de la 
# misma manera que a los |> . Por ejemplo:
# Esto está permitido:
flights |> 
  group_by(month) |> 
  summarize(
    delay = mean(arr_delay, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = month, y = delay)) +
  geom_point() + 
  geom_line()
# Y si una función que conforma las capas un gráfico, tiene demasiados
# argumentos, cada argumento debe ir en su propia línea
flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()

# Puedo usar comentarios de sección para dividir el archivo en partes
# manejables. Por ejemplo: 
# mi primera seccion ------------------------------------------------------
# Este comentario de sección se creó con el comando Ctrl-Shift-R y
# escribiendo "mi primera sección". Y en la parte inferior izquierda
# del editor de script puedo seleccionar la sección a la que quiero 
# ir

########################
###
### 4.6 Ejercicios
###
########################

# 1. Remodele las siguientes tuberías siguiendo las pautas anteriores.
flights |> 
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    n=n(), 
    delay=mean(arr_delay,na.rm=TRUE)
    ) |>
  filter(n > 10)

flights |> 
  filter(
    carrier == "UA",
    dest %in% c("IAH", "HOU"), 
    sched_dep_time > 0900,
    sched_arr_time < 2000) |>
    group_by(flight) |>
  summarize(delay = mean(arr_delay,na.rm=TRUE),
            cancelled = sum(is.na(arr_delay)),
            n = n()
            ) |>
  filter(n > 10)
