##########################################################
##                                                      ##  
##      Análisis explortorio de la variable edad        ##
##                                                      ##
##########################################################

library(tidyverse)
source("r4ds-progreso/obesity-latin/R/load_data.R")

#########################
##
##  0.- Introducción
##
#########################

# Conocer el tipo de la variable edad
class(obesity$Age) # produce: "numeric"

# Conocer los valores de la variable edad
unique(obesity$Age)[1:20] # produce:
#[1] 21 23 27 22 29 24 26 41 30 52 20 19 31 39 17 25 55 38 18 15

# Histograma de la variable edad
ggplot(obesity, aes(x = Age)) +
  geom_histogram(binwidth = 1)

# Gráfico de densidad de la variable edad
ggplot(obesity, aes(x = Age)) +
  geom_density()

######################################################
##
## 1.- Historia familiar de exceso de peso y edad
##
######################################################

# Conocer el tipo de la varaible de famialiares obesos
class(obesity$tiene_familiares_obesos) # produce: "factor"

# Conocer los valores de la varaible de familiares obesos
unique(obesity$tiene_familiares_obesos) # produce: 
#[1] si no
#Levels: si no

# Diagrama de caja que relaciona familiares obesos con la edad, facetada
# por tipo de obesidad
ggplot(obesity, aes(x = tiene_familiares_obesos, y = Age)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla de obesidad por tipo de obesidad y familiares obesos,
# resumido por edad e índice de masa corporal
obesity_familiares <- obesity |>
  group_by(tipo_obesidad, tiene_familiares_obesos) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) 
obesity_familiares# produce:
# A tibble: 11 × 4
# Groups:   tipo_obesidad, tiene_familiares_obesos [11]
#  tipo_obesidad    tiene_familiares_obesos med_imc med_age
#  <fct>            <fct>                     <dbl>   <dbl>
#1 Desnutricion     si                         17.7    19.6
#2 Desnutricion     no                         17.3    19.1
#3 Peso normal      si                         22.5    21  
#4 Peso normal      no                         22.0    20  
#5 Sobrepeso        si                         27.3    22.9
#6 Sobrepeso        no                         26.6    21.8
#7 Obesidad grado 1 si                         32.3    23  
#8 Obesidad grado 1 no                         31.2    23  
#9 Obesidad grado 2 si                         36.9    26.0
#10 Obesidad grado 2 no                         35.0    25  
#11 Obesidad grado 3 si                         42.8    22.8

# Diagrama de dispersión que relaciona el índice de masa corporal 
# con la edad, facetado por tipo de obesidad y coloreado por familiares
# obesos
ggplot(obesity_familiares, aes(x = med_imc, y = med_age)) +
  geom_point(aes(color = tiene_familiares_obesos)) +
  facet_wrap(~tipo_obesidad)

##############################################
##
## 2.- Consumo de comida calórica y Edad
##
##############################################

# Conocer el tipo de la variable de consumo de comida muy calórica
class(obesity$come_comida_muy_calorica) # produce: "factor"

# Conocer los valores de la varaible consumo de comida muy calórica
unique(obesity$come_comida_muy_calorica) # produce: [1] no si

# Diagrama de caja que relaciona el consumo de comida muy calórica con
# edad, factada por tipo de obesidad
ggplot(obesity, aes(x = come_comida_muy_calorica, y = Age)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y consumo de comida
# muy calórica, resumida por índice de masa corporal y edad.
obesity_calorica <- obesity |>
  group_by(tipo_obesidad, come_comida_muy_calorica) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_calorica # produce:
# A tibble: 12 × 4
# Groups:   tipo_obesidad, come_comida_muy_calorica [12]
#  tipo_obesidad    come_comida_muy_calorica med_imc med_age
#  <fct>            <fct>                      <dbl>   <dbl>
#1 Desnutricion     si                          17.6    19  
#2 Desnutricion     no                          17.5    20  
#3 Peso normal      si                          22.3    20.1
#4 Peso normal      no                          22.0    22  
#5 Sobrepeso        si                          26.7    23.0
#6 Sobrepeso        no                          28.0    20.7
#7 Obesidad grado 1 si                          32.3    23  
#8 Obesidad grado 1 no                          31.0    21  
#9 Obesidad grado 2 si                          36.9    26.0
#10 Obesidad grado 2 no                          36.1    30  
#11 Obesidad grado 3 si                          42.8    22.8
#12 Obesidad grado 3 no                          40.6    26  

# Diagrama de dispersión que relaciona la edad con el índice de masa 
# corporal, coloreado por consumo de comida muy calórica
ggplot(obesity_calorica, aes(x = med_age, y = med_imc)) +
  geom_point(aes( color = come_comida_muy_calorica)) +
  facet_wrap(~tipo_obesidad)

####################################
##
## 3.- Consumo de Vegetales y Edad
##
####################################

# Conocer el tipo de la variable consumo de vegetales
class(obesity$come_vegetales) # produce: "numeric"

# Conocer los valores de la varaible consumo de vegetales
unique(obesity$come_vegetales)[1:20] # produce: 
#[1] 2.000000 3.000000 1.000000 2.450218 2.880161 2.008760 2.596579
#[8] 2.591439 2.392665 1.123939 2.027574 2.658112 2.886260 2.714447
#[15] 2.750715 1.492500 2.205439 2.059138 2.310423 2.823179

# Diagrama de dispersión que relaciona consumo de vegetales y edad
ggplot(obesity, aes(x = come_vegetales, y = Age)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla de obesidad por tipo de obesidad , resumida por
# consumo de vegetales y edad
obesity_vegetales <- obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    med_vegetales = median(come_vegetales, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_vegetales # produce:
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    med_vegetales med_age
#  <fct>                    <dbl>   <dbl>
#1 Desnutricion              2.70    19.2
#2 Peso normal               2       21  
#3 Sobrepeso                 2       22.7
#4 Obesidad grado 1          2       23  
#5 Obesidad grado 2          2.61    26.0
#6 Obesidad grado 3          3       22.8

# Diagrama de dispersión que relaciona el consumo de vegetales con
# edad, coloreado y facetado por tipo de obesidad
ggplot(obesity_vegetales, aes(x = med_vegetales, y = med_age)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)
