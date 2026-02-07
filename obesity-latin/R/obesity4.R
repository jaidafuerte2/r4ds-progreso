##########################################################
##                                                      ##  
##      Análisis explortorio de la variable género      ##
##                                                      ##
##########################################################

library(tidyverse)
source("r4ds-progreso/obesity-latin/R/load_data.R")

#########################
##
##  0.- Introducción
##
#########################

# Conocer los valores de la variable género
unique(obesity$Gender) # produce: [1] Female Male

# Conocer el tipo de la variable género
class(obesity$Gender) # produce: [1] "factor"

# Gráfico de barras de la variable género
ggplot(obesity, aes(x = Gender)) +
  geom_bar()

######################################################
##
## 1.- Historia familiar de exceso de peso y género
##
######################################################

# Conocer los valores de la variable historia familiar de exceso de
# peso
unique(obesity$tiene_familiares_obesos) # produce: [1] si no

# Conocer el tipo de la varaible historia familiar de obesidad
class(obesity$tiene_familiares_obesos) # produce: [1] "factor"

# Gráfico de barras que relaciona el género con la historia familiar de
# exceso de peso
ggplot(obesity, aes(x = Gender, fill = tiene_familiares_obesos)) +
  geom_bar()

# Agrupar por historia familiar de exceso de peso y género y resumir por
# imc
obesity_historia_familiar <- obesity |>
  group_by(tiene_familiares_obesos, Gender) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 4 × 3
# Groups:   tiene_familiares_obesos, Gender [4]
#tiene_familiares_obesos Gender med_imc
#  <fct>                   <fct>    <dbl>
#1 si                      Male      31.2
#2 si                      Female    31.8
#3 no                      Male      24.4
#4 no                      Female    18.2

# Diagrama de caja que relaciona el género con la mediana del índice de
# masa corporal. Facetada por historia familiar de obesidad
ggplot(obesity_historia_familiar, aes(x = Gender, 
                                      y = med_imc)) +
  geom_boxplot() +
  facet_wrap(~tiene_familiares_obesos)

##############################################
##
## 2.- Consumo de comida calórica y Género
##
##############################################

# Conocer los valores de la varaible consumo de comida muy calórica
unique(obesity$come_comida_muy_calorica) # produce: [1] no si

# Conocer el tipo de la variable consumo de comida muy calórica
class(obesity$come_comida_muy_calorica) # produce: "factor"

# Gráfico de barras que relaciona el Género con el consumo de comida
# calórica y facetada por tipo de obesidad
ggplot(obesity, aes( x = Gender, fill = come_comida_muy_calorica)) +
  geom_bar(position = "fill") +
  facet_wrap(~tipo_obesidad)

# Agrupar por género y consumo de comida muy calórica
obesity_calorica <- obesity |>
  group_by(Gender, come_comida_muy_calorica) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) 
obesity_calorica # produce:
# A tibble: 4 × 3
# Groups:   Gender, come_comida_muy_calorica [4]
#  Gender come_comida_muy_calorica med_imc
#  <fct>  <fct>                      <dbl>
#1 Male   si                          29.6
#2 Male   no                          25.6
#3 Female si                          30.9
#4 Female no                          22.8

# Diagrama de caja que relaciona el género con el índice de masa 
# corporal y coloreado por consumo de comida calórica
ggplot(obesity_calorica, aes(x = Gender, y = med_imc)) +
  geom_boxplot(aes(color = come_comida_muy_calorica))

####################################
##
## 3.- Consumo de Vegetales y Género
##
####################################

# Conocer los valores de la variable consumo de vegetales
unique(obesity$come_vegetales)[1:20] # produce:
#[1] 2.000000 3.000000 1.000000 2.450218 2.880161 2.008760 2.596579
#[8] 2.591439 2.392665 1.123939 2.027574 2.658112 2.886260 2.714447
#[15] 2.750715 1.492500 2.205439 2.059138 2.310423 2.823179

# Conocer el tipo de la variable consumo de vegetales
class(obesity$come_vegetales) # produce: "numeric"

# Gráfico de caja que relaciona el género con el consumo de vegetales
# facetado por tipo de obesidad
ggplot(obesity, aes(x = Gender, y = come_vegetales)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar por género y tipo de obesidad y resumir por índice de masa 
# corporal y consumo de vegetales
obesity_vegetales <- obesity |>
  group_by(Gender, tipo_obesidad) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_vegetales = median(come_vegetales, na.rm = TRUE),
    .groups = "keep"
  )
obesity_vegetales # produce:
#  Gender tipo_obesidad    med_imc med_vegetales
#  <fct>  <fct>              <dbl>         <dbl>
#1 Male   Desnutricion        17.6          2.39
#2 Male   Peso normal         22.5          2   
#3 Male   Sobrepeso           27.2          2   
#4 Male   Obesidad grado 1    32.7          2   
#5 Male   Obesidad grado 2    36.5          2.40
#6 Male   Obesidad grado 3    49.5          3   
#7 Female Desnutricion        17.5          2.88
#8 Female Peso normal         22.0          2   
#9 Female Sobrepeso           26.7          2.23
#10 Female Obesidad grado 1    31.9          2.06
#11 Female Obesidad grado 2    39.0          3   
#12 Female Obesidad grado 3    42.8          3  

# Diagrama de caja que relaciona el tipo de obesidad con el consumo 
# de vegetales y facetado por género
ggplot(obesity_vegetales, aes(x = tipo_obesidad, y = med_vegetales)) +
  geom_boxplot() +
  facet_wrap(~Gender)

################################################
##
## 4.- Número de Comidas Principales y género
##
################################################

# Conocer los valores de la variable de número de comidas principales
unique(obesity$comidas_principales)[1:20] # produce:
#[1] 3.000000 1.000000 4.000000 3.289260 3.995147 1.726260 2.581015
#[8] 1.600812 1.737620 1.105480 2.084600 1.894384 2.857787 3.765526
#[15] 3.285167 3.691226 3.156153 1.079760 3.559841 3.891994

# Conocer el tipo de la variable de número de comidas principales
class(obesity$comidas_principales) # produce: "numeric"

# Diagrama de caja que relaciona el género con el número de comidas
# principales y facetado por tipo de obesidad
ggplot(obesity, aes(x = Gender, y = comidas_principales)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla de obesidad por género y tipo de obesidad; y resumir
# por número de comidas principales
obesity |>
  group_by(Gender, tipo_obesidad) |>
  summarise(
    med_comidas_principales = median(comidas_principales, 
                                     na.rm = TRUE),
    .groups = "keep"
  )

# #####################################
##
## 5.- Consumo de Refrigerios y género
##
#####################################

# Conocer lo valores de la variable de consumo de refrigerios
unique(obesity$come_refrigerios) # produce:
#[1] ocasional frecuente siempre   nunca

# Conocer el tipo de la variable consumo de refrigerios
class(obesity$come_refrigerios) # produce: "factor"

# Gráfico de barras que relaciona el género con el consumo de 
# refrigerios y facetado por tipo de obeidad
ggplot(obesity, aes(x = Gender, fill = come_refrigerios)) + 
  geom_bar(position = "fill") + 
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por género y consumo de calorías, y 
# resumir por índice de masa corporal
obesity_refrigerios <- obesity |>
  group_by(Gender, come_refrigerios) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  )
obesity_refrigerios # produce:
# A tibble: 8 × 3
# Groups:   Gender, come_refrigerios [8]
#  Gender come_refrigerios med_imc
#  <fct>  <fct>              <dbl>
#1 Male   ocasional           31.0
#2 Male   frecuente           23.0
#3 Male   nunca               26.7
#4 Male   siempre             24.3
#5 Female ocasional           31.4
#6 Female frecuente           18.0
#7 Female nunca               24.5
#8 Female siempre             22.6

# Diagrama de caja que relaciona el consumo de refrigerios con el
# índice de masa corporal y facetada por género
ggplot(obesity_refrigerios, aes(x = come_refrigerios, y = med_imc)) +
  geom_boxplot() +
  facet_wrap(~Gender)

