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
dplyr::near(x, c(1, 2))

