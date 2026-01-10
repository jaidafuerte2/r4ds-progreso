######################
##                  ## 
##      GENDER      ##
##                  ##
######################


library(tidyverse)
source("r4ds-progreso/obesity-risk/R/load_data.R")
#View(obesity)
#getwd()

#########################
##
##  0.- Introducción
##
#########################

# Gráfico de barras de género
ggplot(obesity, aes(x = Gender)) +
  geom_bar()

# Gráfico de barras de historia familiar de exceso de peso
ggplot(obesity, aes(x = family_history_with_overweight)) +
  geom_bar()

#######################################
##
## 1.- Historia familiar de sobrepeso
##
#######################################

# Valores que puede tomar la variable de historia familiar de exceso
# de peso
unique(obesity$family_history_with_overweight) # produce:
#[1] 1 0
# Tipo de la variable de hostoria familiar de exceso de peso 
obesity |> count(family_history_with_overweight) # produce:
#family_history_with_overweight     n
#                           <dbl> <int>
#1                              0  3744
#2                              1 17014

# Diagrama de caja que relaciona género con imc
ggplot(obesity, aes(x = Gender, y = imc)) +
  geom_boxplot()

# Gráfico de barras apiladas para relacionar género con historia familiar
# de exceso de eso
ggplot(obesity, aes(x = Gender, 
                    fill = family_history_with_overweight)) +
  geom_bar()

# Gráfico de barras apiladas de frecuencias relaticas para relacionar
# género con historia familiar de obesidad
ggplot(obesity, aes(x = Gender, 
                    fill = family_history_with_overweight)) +
  geom_bar(position = "fill")

################################
##
## 2.- Consumo de comida calórica
##
################################

# Conocer las categorías de consumo frecuente de comida altamente
# caloríca
unique(obesity$frequent_high_calorie_food) # produce:
# [1] Si No

# Conocer el tipo de la variable consume frecuente de comida altamente
# calórica
class(obesity$frequent_high_calorie_food) # produce:
#[1] "factor"

# Gráfico de barras apiladas de Género en relación con el consumo 
# frecuente de comida altamente calórica
ggplot(obesity, aes(x = Gender, fill = frequent_high_calorie_food)) +
  geom_bar()

# Gráfico de barras apiladas de frecuencias relativas de Género en 
# relación con el consumo frecuente de comida altamente calórica
ggplot(obesity, aes(x = Gender, fill = frequent_high_calorie_food)) +
  geom_bar(position = "fill")

####################################
##
## 3.- Consumo de Vegetales
##
####################################

# Conocer el tipo de valores de la frecuencia de consumo de vegeteales
unique(obesity$vegetable_consumption_frequency) # produce:
#[1] 2.000000 1.880534 3.000000 2.679664 2.919751 1.991240 1.397468
#[8] 2.636719 1.000000 1.392665 2.203962 2.971588 2.668949 1.989899
#[15] 2.417635 2.219186 2.919526 2.263245 2.649406 1.754401 2.303656
#[22] 2.020785 2.068834 2.689929 2.979383 2.225731 2.843456 2.312528
#[29] 2.962415 2.945967 2.108638 1.826885 2.200588 2.598051 2.984425

class(obesity$vegetable_consumption_frequency) # produce:
#[1] "numeric"

# Diagrama de caja que relaciona género con frecuencia de consumo de 
# vegetales
ggplot(obesity, aes(x = Gender, y = vegetable_consumption_frequency)) +
  geom_boxplot()

ggplot(obesity, aes(x = vegetable_consumption_frequency, 
                    color = Gender)) +
  geom_density(linewidth = 0.75)
# NOTA: Aquí parece haber diferencias marcadas porque parece que las 
# mujeres comen más vegetales que los hombres.

# Diagrama de dispersión que relaciona imc con frecuencia de consumo
# de vegetales por género
ggplot(
  data = obesity,
  mapping = aes(x = vegetable_consumption_frequency, y = imc,
                color = Gender)
) +
  geom_point()

# Facetar por consumo de vegetales e imc en relación y facetado por 
# género
ggplot(obesity, aes(x = vegetable_consumption_frequency, y = imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~Gender)
# NOTA: Parece que sí comen más vegetales los hombres y tienen un imc
# mayor

# Facetar por consumo de vegetales e imc en relación y facetado por 
# tipo de obesidad
ggplot(obesity, aes(x = vegetable_consumption_frequency, y = imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~tipo_obesidad)
# Se observa que mujeres que comen muchos vegetales, también son muy
# obesas

# Agrupar por género y tipo de obesidad y resumir imc y consumo de
# vegetales
obesity_by_gender_tipo <- obesity |>
  group_by(Gender, tipo_obesidad) |>
  summarize(
    avg_vegetable = mean(vegetable_consumption_frequency, 
                         na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE)
  )
obesity_by_gender_tipo # produce:
# A tibble: 12 × 4
# Groups:   Gender [2]
#  Gender tipo_obesidad    avg_vegetable avg_imc
#  <chr>  <fct>                    <dbl>   <dbl>
#1 Female Desnutrición              2.58    17.3
#2 Female Peso normal               2.42    21.8
#3 Female Sobrepeso                 2.30    27.1
#4 Female Obesidad grado 1          2.15    32.0
#5 Female Obesidad grado 2          2.95    38.5
#6 Female Obesidad grado 3          3.00    42.8
#7 Male   Desnutrición              2.29    17.2
#8 Male   Peso normal               2.31    22.4
#9 Male   Sobrepeso                 2.20    27.3
#10 Male   Obesidad grado 1          2.21    32.8
#11 Male   Obesidad grado 2          2.39    37.0
#12 Male   Obesidad grado 3          2.40    41.7

# Diagrama de dispersión que relaciona imc con frecuencia de consumo
# de vegetales, por tipo de obesidad
ggplot(obesity_by_gender_tipo, aes(x = avg_imc, y = avg_vegetable)) +
  geom_point(aes(color = tipo_obesidad))

# Diagrama de dispersión que relaciona imc con frecuencia de consumo
# de vegetales, por tipo de obesidad y facetado por género
ggplot(obesity_by_gender_tipo, aes(x = avg_imc, y = avg_vegetable)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~Gender)
# NOTA: Es notorio que las mujeres con obesidad grado 2 y obesidad
# grado 3 comen más vegetales que los hombres. Parece que comer
# muchos vegetales no les protege de obesidad

######################################
##
## 4.- Número de Comidas Principales
##
######################################

# Conocer el tipo de valores que tienen la variable de número de 
# comidas principales
unique(obesity$number_main_meals) # produce:
#[1] 2.983297 3.000000 1.411685 1.971472 2.164839 1.000000 2.954446
#[8] 1.893811 3.998618 1.703299 2.937989 2.996444 2.581015 2.473913
#[15] 1.437959 2.989791 4.000000 2.853676 1.104642 3.362758 1.169173
#[22] 1.411808 2.982120 1.816980 3.762778 2.976211 2.993623 3.994588
#[29] 3.087544 2.372311 2.376374 2.884479 2.994198 2.812283 3.654061

# Conocer el tipo de la varaible de número de comidas principales
class(obesity$number_main_meals) # produce:
#[1] "numeric"

ggplot(obesity, aes(x = Gender, y = number_main_meals)) +
  geom_boxplot()

ggplot(obesity, aes(x = number_main_meals, 
                    color = Gender)) +
  geom_density(linewidth = 0.75)

#####################################
##
## 5.- Número de Refrigerios
##
#####################################

# Conocer el tipo de observaciones de refrigerios
unique(obesity$food_between_meals) # produce:
#[1] "Sometimes"  "Frequently" "0"          "Always"  

# Conocer el tipo de la variable refrigerios
class(obesity$food_between_meals)  # produce:
#[1] "factor"

ggplot(obesity, aes(x = Gender, fill = food_between_meals)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")

#####################################
##
## 6.- Fumar
##
#####################################

# Muestra los valores de la variable SMOKE
unique(obesity$SMOKE) # produce:

# Muestra el tipo de la variable SMOKE
class(obesity$SMOKE) # produce:
# [1] "factor"

ggplot(obesity, aes(x = Gender, fill = SMOKE)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")

##################################
##
## 7.- Consumo de Agua
##
##################################

# Visualizar el tipo de valores que tiene la variable consumo de agua
unique(obesity$daily_water_consumption) # produce:
#[1] 2.763573 2.000000 1.910378 1.674061 1.979848 2.137550 3.000000
#[8] 2.632253 2.530157 1.959531 1.000000 1.238057 2.724099 2.072194
#[15] 2.609052 2.487070 2.854161 2.632224 1.726109 2.939492 2.869234
#[22] 1.438398 2.371015 1.509734 2.109697 2.770125 2.786780 1.937674

# Conocer el tipo de la variable consumo de agua
class(obesity$daily_water_consumption) # produce:
#[1] "numeric"

ggplot(obesity, aes(x = Gender, y = daily_water_consumption)) +
  geom_boxplot()

ggplot(obesity, aes(x = daily_water_consumption, 
                    color = Gender)) +
  geom_density(linewidth = 0.75)

# Diagrama de dispersión que relaciona imc con consumo de agua 
# por género
ggplot(
  data = obesity,
  mapping = aes(x = daily_water_consumption, y = imc,
                color = Gender)
) +
  geom_point()

# Facetar por consumo de agua e imc en relación y facetado por 
# género
ggplot(obesity, aes(x = daily_water_consumption, y = imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~Gender)

# Facetar por consumo de agua e imc en relación y facetado por 
# tipo de obesidad
ggplot(obesity, aes(x = daily_water_consumption, y = imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~tipo_obesidad)

#####################################
##
## 8.- Consumo de bebidas calóricas
##
#####################################

# Conocer el tipo de valores que puede tomar la variable de consumo de
# bebidas calóricas
unique(obesity$caloric_beverages_consumption) # produce:
#[1] 0 1

# Conocer el tipo de la variable consumo de bebidas calóricas
class(obesity$caloric_beverages_consumption) # produce:
# integer

ggplot(obesity, aes(x = Gender, fill = caloric_beverages_consumption)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")

####################################
##
## 9.- Actividad Física
##
####################################

unique(obesity$physical_activity_frequency) # produce:
#[1] 0.0000000 1.0000000 0.8660450 1.4678630 1.9679730 1.9300330
#[7] 0.5986550 2.0000000 1.4257120 3.0000000 1.9955820 1.0979050
#[13] 0.6804640 1.1910200 1.9998360 1.4659310 0.8266600 0.0359280
#[19] 1.6286370 1.4274130 0.5447840 0.2820630 0.9797010 1.1904650
#[25] 0.4638910 1.8180520 0.6921230 1.3609940 1.5017540 0.0836750

# Conocer el tipo de la variable frecuencia de actividad física
class(obesity$physical_activity_frequency) # produce:
# numeric

# Diagrama de caja que relaciona la densidad de la frecuencia de 
# actividad física con el género
ggplot(obesity, aes(x = Gender, y = physical_activity_frequency)) +
  geom_boxplot()
# NOTA: La mediana de actividad física es ligeramente menor en mujeres
# pero su rango intercuartil inferior al 50% también es bastante menor
# en las mujeres. Vale la pena investigar

# Gráfico de densidad que ralciona la actividad física con el género
ggplot(obesity, aes(x = physical_activity_frequency,
                    color = Gender)) +
  geom_density()
# Se observa cláramente que hay más mujeres que no hacen nada de 
# actividad física, posteriormente es mayor la actividad física
# que hacen los hombres. Vale la pena investigar


ggplot(obesity, aes(x = physical_activity_frequency, y = imc)) +
  geom_point(aes(color = Gender))

# Diagrama de dispersión que relaciona actividad física e imc por
# género y facetado por tipo de obesidad
ggplot(obesity, aes(x = physical_activity_frequency, y = imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~tipo_obesidad)
# NOTA: Otra vez se ve que las mujeres con obesidad grado 3 hacen 
# mucho más ejercicio que los hombres , mientras que los hombres
# con obesidad grado 1 y 2 hacen más ejercicio que las mujeres.

# Agrupar por género y tipo de obesidad y resumir la actividad física
# y el imc
obesity_by_gender_tipo_physical <- obesity |>
  group_by(Gender, tipo_obesidad) |>
  summarise(
    avg_physical_activity = mean(physical_activity_frequency,
                                 na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE)
  )
obesity_by_gender_tipo_physical # produce:
# A tibble: 12 × 4
# Groups:   Gender [2]
#   Gender tipo_obesidad    avg_physical_activity avg_imc
#   <chr>  <fct>                            <dbl>   <dbl>
#1 Female Desnutrición                     1.09     17.3
#2 Female Peso normal                      1.04     21.8
#3 Female Sobrepeso                        0.889    27.1
#4 Female Obesidad grado 1                 0.617    32.0
#5 Female Obesidad grado 2                 0.176    38.5
#6 Female Obesidad grado 3                 0.659    42.8
#7 Male   Desnutrición                     1.36     17.2
#8 Male   Peso normal                      1.39     22.4
#9 Male   Sobrepeso                        1.23     27.3
#10 Male   Obesidad grado 1                 1.11     32.8
#11 Male   Obesidad grado 2                 1.01     37.0
#12 Male   Obesidad grado 3                 0.906    41.7

# Diagrama de dispersión que relaciona imc con actividad física,
# por tipo de obesidad
ggplot(obesity_by_gender_tipo_physical, 
       aes(x = avg_imc, y = avg_physical_activity)) +
  geom_point(aes(color = tipo_obesidad))

# Diagrama de dispersión que relaciona imc con actividad física,
# por tipo de obesidad y facetado por género
ggplot(obesity_by_gender_tipo_physical, 
       aes(x = avg_imc, y = avg_physical_activity)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~Gender)
# NOTA: Aquí es muy notorio que las mujeres más obesas sí hacen 
# mucho ejercicio. PILAS

################################
##
## Tiempo de Uso de Tecnología 
##
################################

# Conocer el tipo de valores que tiene la variable tiempo de uso de 
# tecnología
unique(obesity$technology_use_time) # produce:
#[1] 0.976473 1.000000 1.673584 0.780199 0.931721 0.696948 0.000000
#[8] 0.218645 0.553311 0.947884 2.000000 0.930836 0.619012 0.081156
#[15] 1.258881 0.079334 0.250502 0.232858 0.453649 0.831412 0.704978
#[22] 0.929356 0.868788 0.097760 0.425473 0.773807 1.544357 1.239038
#[29] 0.479221 0.631217 0.301909 0.625350 0.453404 0.579541 0.939726

class(obesity$technology_use_time) # produce:
# numeric

ggplot(obesity, aes(x = Gender, y = technology_use_time)) +
  geom_boxplot()

ggplot(obesity, aes(x = technology_use_time, color = Gender)) +
  geom_density()

###########################
##
## Consumo de Alcohol
##
###########################

# Conocer el tipo de valores que tiene la variable de consumo de 
# alcohol
unique(obesity$alcohol_consumption) # produce:
#[1] "Sometimes"  "0"          "Frequently"

class(obesity$alcohol_consumption) # produce:
#"factor"

# Gráfico de barras apiladas relacionando género con consumo de
# alcohol
ggplot(obesity, aes(x = Gender, fill = alcohol_consumption)) +
  geom_bar(position = "fill") +
  labs(y = "proportions")
# NOTA: Parece ser que hay más mujeres bebedoras sociales y más
# hombres que nunca toman 

# Gráfico de densidad que relaciona el imc con el consumo de 
# alcohol y facetado por género
ggplot(obesity, aes(x = imc, color = alcohol_consumption)) +
  geom_density(linewidth = 0.75) +
  facet_wrap(~Gender)

