######################
##                  ## 
##        AGE       ##
##                  ##
######################

library(tidyverse)
source("r4ds-progreso/obesity-risk/R/load_data.R")
View(obesity)
#getwd()

#########################
##
##  0.- Introducción
##
#########################

# Diagrama de dispersión que relaciona la edad con el imc
ggplot(obesity, aes(x = imc, y = Age)) +
  geom_point()

# Diagrama de caja que relaciona la edad con el tipo de obesidad
ggplot(obesity, aes(x = tipo_obesidad, y = Age)) +
  geom_boxplot()

# Agrupar por tipo de obesidad y resumir por edad
obesity |>
  group_by(tipo_obesidad) |>
  summarize(
    avg_age = mean(Age, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE)
  )
# Nota: se observa que los más obesos si tienen una edad ligeramente 
# mayor

##########################
##
##     1.- Género
##
##########################

# Conocer el tipo de valores que tiene la variable Gender
unique(obesity$Gender) # produce:
# [1] "Male"   "Female"

# Conocer el tipo de la variable Gender
class(obesity$Gender) # produce:
# "factor"

# Diagrama de caja que relaciona el género con la edad
ggplot(obesity, aes(x = Gender, y = Age)) +
  geom_boxplot()

# mutar la tabla para saber los que son mayores o iguales a 45 años
obesity_45years <- obesity |>
  mutate(
    more_45y = Age >= 45,
    .before = 1
  )
obesity_45years # produce:
# A tibble: 20,758 × 21
#  more_45y    id Gender   Age Height Weight family_history_with_overwe…¹
#  <lgl>    <dbl> <fct>  <dbl>  <dbl>  <dbl> <fct>                       
#1 FALSE        0 Male    24.4   1.70   81.7 Si                          
#2 FALSE        1 Female  18     1.56   57   Si                          
#3 FALSE        2 Female  18     1.71   50.2 Si                          
#4 FALSE        3 Female  21.0   1.71  131.  Si

# Gráfico de barras que relaciona el género con las personas mayores
# a cuarenta y cinco años
ggplot(obesity_45years, aes(x = Gender, fill = more_45y)) +
  geom_bar()

# Gráfico de densidad que relaciona el imc con las personas mayores
# a cuarenta y cinco años y  facetado por género
ggplot(obesity_45years, aes(x = imc, color = more_45y)) +
  geom_density() +
  facet_wrap(~Gender)

# Agrupar la tabla de obesidad por tipo de obesidad y personas mayores
# a 45 años y resumir por imc
obesity_45_tipo <- obesity_45years |>
  group_by(tipo_obesidad, more_45y) |>
  summarize(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "drop"
  )

# Diagrama de caja que relaciona las personas mayores de 45 años con 
# el índice de masa corporal y facetado por tipo de obesidad
ggplot(obesity_45_tipo, aes(x = more_45y, y = med_imc)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla mutada de obesidad por género y personas mayores a
# cuarenta y cinco años y resumido por índice de masa corporal
obesity_45_gender <- obesity_45years |>
  group_by(more_45y, Gender) |>
  summarize(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "drop"
  )
obesity_45_gender # produce:

# Diagrama de caja que relaciona a las personas mayores a 45 años
# con la mediana del índice de masa corporal
ggplot(obesity_45_gender, aes(x = more_45y, y = med_imc)) +
  geom_boxplot()
# Este gráfico es definitorio. Las personas mayores de 45 años tienen 
# un imc menor que los menores de 45 años

# Diagrama de caja que relaciona a las personas mayores a 45 años
# con la mediana del índice de masa corporal y facetada por género
ggplot(obesity_45_gender, aes(x = more_45y, y = med_imc)) +
  geom_boxplot() +
  facet_wrap(~Gender)

###########################
##
##   2.- Historia Familiar  
##
###########################

# Conocer el tipo de valores que tienela variable historia familiar
# de exceso de peso
unique(obesity$family_history_with_overweight) # produce:
#[1] Si No

# Conocer el tipo de la variable historia familiar de exceso de peso
class(obesity$family_history_with_overweight) # produce:
# "factor"

# Diagrama de caja que relaciona la historia familiar de exceso de 
# peso con la edad
ggplot(obesity, aes(x = family_history_with_overweight, y = Age)) +
  geom_boxplot()

#####################################
##
## 3.- Frecuencia de comida calórica
##
#####################################

# Conocer el tipo de valores que tiene la variable frecuencia de comida
# muy calórica
unique(obesity$frequent_high_calorie_food) # produce:
# [1] Si No

# Conocer el tipo de la variable frecuencia de comida muy calórica
class(obesity$frequent_high_calorie_food) # produce:
# "factor"

# Diagrama de caja que relaciona la edad con la frecuencia de comida
# muy calórica
ggplot(obesity, aes(x = frequent_high_calorie_food, y = Age)) +
  geom_boxplot()

############################
##
## 4.- Consumo de Vegetales
##
############################

# Conocer el tipo de valores que tiene la varaible consumo de vegetales
unique(obesity$vegetable_consumption_frequency) # produce:
#[1] 2.000000 1.880534 3.000000 2.679664 2.919751 1.991240 1.397468
#[8] 2.636719 1.000000 1.392665 2.203962 2.971588 2.668949 1.989899
#[15] 2.417635 2.219186 2.919526 2.263245 2.649406 1.754401 2.303656
#[22] 2.020785 2.068834 2.689929 2.979383 2.225731 2.843456 2.312528

# Conocer el tipo de la variable consumo de vegetales
class(obesity$vegetable_consumption_frequency) # produce:
# "numeric"

# Diagrama de dispersión que relaciona edad con consumo de vegetales
# y facetado por tipo de obesidad
ggplot(obesity, aes(x = vegetable_consumption_frequency, y = Age)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar por tipo de obesidad y resumir por edad y consumo de 
# vegetales
obesity_vegetable <- obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    avg_vegetable = mean(vegetable_consumption_frequency, 
                           na.rm = TRUE),
    med_vegetable = median(vegetable_consumption_frequency,
                           na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),#
    #tipo_obesidad = tipo_obesidad
    .groups = "drop"
  )

obesity_vegetable # produce: 
# A tibble: 6 × 5
#  tipo_obesidad    avg_vegetable med_vegetable avg_age med_age
#  <fct>                    <dbl>         <dbl>   <dbl>   <dbl>
#1 Desnutrición              2.47          2.75    19.3    19  
#2 Peso normal               2.37          2       21.0    21  
#3 Sobrepeso                 2.23          2       24.5    22  
#4 Obesidad grado 1          2.19          2       25.6    23  
#5 Obesidad grado 2          2.52          2.72    27.2    26.0
#6 Obesidad grado 3          2.98          3       23.8    26.0

# Diagrama de dispersión que relaciona consumo de vegetales con edad
# coloreado por tipo de obesidad
ggplot(obesity_vegetable, aes(x = med_vegetable, y = med_age)) +
  geom_point(aes(color = tipo_obesidad))

# Diagrama de densidad que relaciona el consumo de vegetales con
# el tipo de obesiddad
ggplot(obesity, aes(x = vegetable_consumption_frequency, 
                    color = tipo_obesidad)) +
  geom_density() +
  facet_wrap(~tipo_obesidad)


############################
##
## 5.- Número de Comidas
##
############################

# Conocer el tipo de valores que tiene la variable número de comidas
# principales
unique(obesity$number_main_meals) # produce:
#[1] 2.983297 3.000000 1.411685 1.971472 2.164839 1.000000 2.954446
#[8] 1.893811 3.998618 1.703299 2.937989 2.996444 2.581015 2.473913
#[15] 1.437959 2.989791 4.000000 2.853676 1.104642 3.362758 1.169173
#[22] 1.411808 2.982120 1.816980 3.762778 2.976211 2.993623 3.994588

# Conocer el tipo de la variable número de comidas principales
class(obesity$number_main_meals) # produce:
#"numeric"

# Gráfico de dispersión que relaciona el número de comidas principales
# con la edad y facetado por tipo de obesidad
ggplot(obesity, aes(x = number_main_meals, y = Age)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Gráfico de demsidad que relaciona el número de comidas principales
# con el tipo de obesidad y facetado por tipo de obesidad
ggplot(obesity, aes(x = number_main_meals, color = tipo_obesidad)) +
  geom_density() +
  facet_wrap(~tipo_obesidad)

# Agrupar por tipo de obesidad y resumir por mediana de comidas y edad
obesity_meals <- obesity |>
  group_by(tipo_obesidad) |>
  summarize(
    avg_meals = mean(number_main_meals, na.rm = TRUE),
    med_meals = median(number_main_meals, na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "drop"
  ) # produce:
obesity_meals # produce:
# A tibble: 6 × 5
#  tipo_obesidad    avg_meals med_meals avg_age med_age
#  <fct>                <dbl>     <dbl>   <dbl>   <dbl>
#1 Desnutrición          2.89         3    19.3    19  
#2 Peso normal           2.86         3    21.0    21  
#3 Sobrepeso             2.56         3    24.5    22  
#4 Obesidad grado 1      2.50         3    25.6    23  
#5 Obesidad grado 2      2.85         3    27.2    26.0
#6 Obesidad grado 3      2.99         3    23.8    26.0

#####################################
##
## 6.- Entrecomidas
##
#####################################

# Conocer el tipo de variables que tiene la variable entrecomidas
unique(obesity$food_between_meals) # produce:
# [1] Sometimes  Frequently 0          Always 

# Conocer el tip de la variable entrecomidas
class(obesity$food_between_meals) # produce:
# "factor"

# Diagrama de caja que relaciona la edad con a variable entre comidas 
ggplot(obesity, aes(x = food_between_meals, y = Age )) +
  geom_boxplot()


obesity_between_meal <- obesity |>
  group_by(tipo_obesidad, food_between_meals) |>
  summarise(
    avg_imc = mean(imc, na.rm = TRUE),
    med_imc = median(imc, na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "drop"
  )
print(obesity_between_meal, n = Inf) # produce:
#A tibble: 23 × 6
#  tipo_obesidad    food_between_meals avg_imc med_imc avg_age med_age
#  <fct>            <fct>                <dbl>   <dbl>   <dbl>   <dbl>
#1 Desnutrición     Sometimes             17.2    17.4    18.7    18  
#2 Desnutrición     Frequently            17.3    17.4    20.0    19.7
#3 Desnutrición     0                     17.5    17.4    20.2    21  
#4 Desnutrición     Always                17.2    17.5    19.4    18  
#5 Peso normal      Sometimes             22.1    22.1    21.0    21  
#6 Peso normal      Frequently            21.8    21.9    20.8    20  
#7 Peso normal      0                     22.9    23.4    21.4    21  
#8 Peso normal      Always                22.2    21.9    21.0    21  
#9 Sobrepeso        Sometimes             27.3    27.3    24.8    22.1
#10 Sobrepeso        Frequently            27.0    26.9    23.2    21  
#11 Sobrepeso        0                     26.5    26.6    21.3    21  
#12 Sobrepeso        Always                27.1    26.9    24.0    22  
#13 Obesidad grado 1 Sometimes             32.5    32.5    25.6    23  
#14 Obesidad grado 1 Frequently            32.0    32.0    24.3    23  
#15 Obesidad grado 1 0                     32.3    31.2    21.7    21  
#16 Obesidad grado 1 Always                32.6    32.9    25.6    23  
#17 Obesidad grado 2 Sometimes             37.3    37.1    27.2    26.0
#18 Obesidad grado 2 Frequently            36.6    36.5    25.4    22.7
#19 Obesidad grado 2 0                     36.1    35.8    25.3    24  
#20 Obesidad grado 2 Always                37.1    36.4    26.9    25  
#21 Obesidad grado 3 Sometimes             42.8    42.1    23.8    26.0
#22 Obesidad grado 3 Frequently            42.4    42.3    24.0    22.0
#23 Obesidad grado 3 Always                40.8    40.8    31      31  

# Diagrama de dispersión que relaciona la mediana de imc
ggplot(obesity_between_meal, aes(x = med_imc, y = med_age)) +
  geom_point(aes(color = food_between_meals)) +
  facet_wrap(~tipo_obesidad)
# Algo que es interesante es que al observar las medianas, vemos
# que las personas que no comen entre comidas tienen un mayor imc
# dentro de las personas de peso normal (pero siguen siendo de peso
# normal), pero dentro de los grupos de los grupo de exceso de peso
# los que no comen postres o entrecomidas tienen un imc menor dentro
# de sus grupos.Y es más, las personas en el grupo de obesidad 
# grado 3 no hay quienes no comen postres 


#####################################
##
## 7.- Smoke
##
#####################################

# Conocer el tipo de valores que tiene la variable fumar
unique(obesity$SMOKE) # produce:
# [1] No Si

# Conocer el tipo de la variables fumar
class(obesity$SMOKE) # produce: "factor"

# Diagrama de caja que relaciona la variable fumar con la edad
ggplot(obesity, aes(x = SMOKE, y = Age)) +
  geom_boxplot() # produce:

# Diagrama de caja que relaciona la edad con fumar y facetado por tipo
# de obesidad
ggplot(obesity, aes(x = Age, color = SMOKE)) +
  geom_density() +
  facet_wrap(~tipo_obesidad)

obesity_SMOKE <- obesity |>
  group_by(tipo_obesidad, SMOKE) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "drop"
  )
obesity_SMOKE # produce:
# A tibble: 12 × 4
#   tipo_obesidad    SMOKE med_imc med_age
#   <fct>            <fct>   <dbl>   <dbl>
#1  Desnutrición     No       17.4    19  
#2  Desnutrición     Si       17.4    23  
#3  Peso normal      No       22.1    21  
#4  Peso normal      Si       23.6    21  
#5  Sobrepeso        No       27.1    22.0
#6  Sobrepeso        Si       26.8    22.9
#7  Obesidad grado 1 No       32.5    23  
#8  Obesidad grado 1 Si       32.6    26.0
#9  Obesidad grado 2 No       37.1    26.0
#10 Obesidad grado 2 Si       36.6    30.9
#11 Obesidad grado 3 No       42.1    26.0
#12 Obesidad grado 3 Si       40.8    30.8

# Diagrama de dispersión que relaciona las medianas del imc y la 
# edad, coloreado por el hábito de fumar y facetado por tipo de 
# obesidad
ggplot(obesity_SMOKE, aes(x = med_imc, y = med_age)) +
  geom_point(aes(color = SMOKE)) +
  facet_wrap(~tipo_obesidad)

#####################################
##
## 8.- Consumo de agua
##
#####################################

# Conocer el tipo de valores de la variable consumo de agua
unique(obesity$daily_water_consumption) # produce:
#[1] 2.763573 2.000000 1.910378 1.674061 1.979848 2.137550 3.000000
#[8] 2.632253 2.530157 1.959531 1.000000 1.238057 2.724099 2.072194
#[15] 2.609052 2.487070 2.854161 2.632224 1.726109 2.939492 2.869234
#[22] 1.438398 2.371015 1.509734 2.109697 2.770125 2.786780 1.937674

# Conocer el tipo de la varaible consumo de agua
class(obesity$daily_water_consumption) # produce: "numeric"

# Diagrama de dispersión que relaciona la edad con el consumo de agua,
# coloreado y facetado por tipo de obesidad
ggplot(obesity, aes(x = daily_water_consumption, y = Age)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar por tipo de obesidad y resumir por consumo de agua, edad
# e imc
obesity_water <- obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    med_water = median(daily_water_consumption, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    med_imc = median(imc, na.rm = TRUE),
    .groups = "drop"
  )
obesity_water # produce:
# A tibble: 6 × 4
#  tipo_obesidad    med_water med_age med_imc
#  <fct>                <dbl>   <dbl>   <dbl>
#1 Desnutrición          2       19      17.4
#2 Peso normal           2       21      22.1
#3 Sobrepeso             2       22      27.1
#4 Obesidad grado 1      2       23      32.5
#5 Obesidad grado 2      2.02    26.0    37.1
#6 Obesidad grado 3      2.63    26.0    42.1

# Diagrama de sispersión que relaciona el consumo de agua con edad
# y coloreado por tipo de obesidad
ggplot(obesity_water, aes(x = med_water, y = med_age)) +
  geom_point(aes(color = tipo_obesidad))

#####################################
##
## 8.- Consumo de bebidas calóricas
##
#####################################

# Conocer el tipo de valores que tiene la variable consumo de bebidas
# calóricas
unique(obesity$caloric_beverages_consumption) # produce: 
# [1] No Si

# Conocer el tipo de la variable consumo de bebidas calóricas
class(obesity$caloric_beverages_consumption) # produce: "factor" 

# Diagrama de caja que relaciona la edad con el consumo de bebidas
# calóricas
ggplot(obesity, aes(x = caloric_beverages_consumption, y = Age)) +
  geom_boxplot() 

# Gráfico de densidad que relaciona la edad con el consumo de bebidas
# calóricas
ggplot(obesity, aes(x = Age, color = caloric_beverages_consumption)) +
  geom_density()

# Agrupar por tipo de obesidad y consumo de bebidas calóricas y resumir
# por imc y edad
obesity_caloric_beverages <- obesity |>
  group_by(tipo_obesidad, caloric_beverages_consumption) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "drop"
  )
obesity_caloric_beverages

# Diagrama de dispersión que relaciona la edad con el imc, coloreado 
# por consumo de bebidas calóricas y facetado por tipo de obesidad
ggplot(obesity_caloric_beverages, aes(x = med_imc,
                                      y = med_age)) +
  geom_point(aes(color = caloric_beverages_consumption)) +
  facet_wrap(~tipo_obesidad)
# Es interesante ver que los que no toman bebidas calóricas tienen 
# un mayor imc.