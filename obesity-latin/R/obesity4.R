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

#####################################
##
## 6.- Hábito de Fumar y Género
##
#####################################

# Conocer los valores de la varible de hábito de fumar
unique(obesity$fuma) # produce: [1] no si

# Conocer el tipo de la variable hábito de fumar
class(obesity$fuma) # produce: [1] "factor"

# Gráfico de barras que relaciona género con el hábito de fumar y 
# facetado por tipo de obesidad
ggplot(obesity, aes(x = Gender, fill = fuma)) +
  geom_bar(position = "fill") +
  facet_wrap(~tipo_obesidad)

obesity |>
  group_by(Gender, fuma) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  )
# A tibble: 4 × 4
# Groups:   Gender, fuma [4]
#  Gender fuma  med_imc avg_imc
#  <fct>  <fct>   <dbl>   <dbl>
#1 Male   si       35.3    31.1
#2 Male   no       28.8    29.2
#3 Female si       26.9    26.8
#4 Female no       28.5    30.2

##################################
##
## 7.- Consumo de Agua y Género
##
##################################

# Conocer el tipo de la variable consumo de agua
class(obesity$agua_diaria) # produce: "numeric"

# Conocer los valores de la varaible consumo de agua
unique(obesity$agua_diaria)[1:20] # produce:
#[1] 2.000000 3.000000 1.000000 1.152736 1.115967 2.704507 2.184707
#[8] 2.406541 2.984323 2.444125 2.654702 2.825629 2.847264 2.884033
#[15] 2.147746 2.815293 2.593459 1.031354 2.651258 1.792022

# Diagrama de caja que relaciona el género con el consumo de agua
# y facetado por tipo de obesidad
ggplot(obesity, aes(x = Gender, y = agua_diaria)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla de obesidad por tipo de obesidad y género y resumida
# por consumo de agua e índice de masa corporal
obesity_agua <- obesity |>
  group_by(tipo_obesidad, Gender) |>
  summarise(
    med_agua = median(agua_diaria, na.rm = TRUE),
    avg_agua = mean(agua_diaria, na.tm = TRUE),
    med_imc = median(imc, na.rm = TRUE),
    .groups = "keep"
  )
obesity_agua # produce
# A tibble: 12 × 5
# Groups:   tipo_obesidad, Gender [12]
#  tipo_obesidad    Gender med_agua avg_agua med_imc
#  <fct>            <fct>     <dbl>    <dbl>   <dbl>
#1 Desnutricion     Male       2        2.12    17.6
#2 Desnutricion     Female     1.67     1.72    17.5
#3 Peso normal      Male       2        1.93    22.5
#4 Peso normal      Female     2        1.77    22.0
#5 Sobrepeso        Male       2        2.15    27.2
#6 Sobrepeso        Female     2        1.92    26.7
#7 Obesidad grado 1 Male       2.36     2.31    32.7
#8 Obesidad grado 1 Female     1.94     1.85    31.9
#9 Obesidad grado 2 Male       2        1.86    36.5
#10 Obesidad grado 2 Female     1.50     1.75    39.0
#11 Obesidad grado 3 Male       2        2       49.5
#12 Obesidad grado 3 Female     2.50     2.30    42.8

# Diagrama de dispersión que relaciona el consumo de agua con el 
# índice de masa corporal, colorea por género y faceta por tipo de 
# obesidad
ggplot(obesity_agua, aes(x = med_agua, y = med_imc)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~tipo_obesidad)

###############################################
##
## 8.- Consumo de bebidas calóricas y género
##
###############################################

# Conocer el tipo de las variables consumo de bebidas calóricas
class(obesity$toma_bebidas_calóricas) # produce: "factor"

# Conocer los valores de la variable consumo de bebidas calóricas
unique(obesity$toma_bebidas_calóricas) # produce: [1] no si

# Gráfico de barras que relaciona el género con el consumo de bebidas
# calóricas, facetado por tipo de obesidad
ggplot(obesity, aes(x = Gender, fill = toma_bebidas_calóricas)) +
  geom_bar(position = "fill") +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por género y consumo de bebidas calóricas
# y facetar por índice de masa corporal 
obesity |>
  group_by(Gender, toma_bebidas_calóricas) |>
  summarize(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 4 × 4
# Groups:   Gender, toma_bebidas_calóricas [4]
#  Gender toma_bebidas_calóricas med_imc avg_imc
#  <fct>  <fct>                    <dbl>   <dbl>
#1 Male   no                        29.0    29.4
#2 Male   si                        24.8    24.5
#3 Female no                        29.4    30.7
#4 Female si                        23.6    22.4

####################################
##
## 9.- Actividad Física y Género
##
####################################

# Conocer el tipo de la varaible actividad física
class(obesity$actividad_fisica) # produce: "numeric"

# Conocer los valores de la variable actividad física
unique(obesity$actividad_fisica)[1:20] # produce: 
#[1] 0.000000 3.000000 2.000000 1.000000 0.319156 1.541072 1.978631
#[8] 0.100320 1.586525 1.399183 1.680844 2.206738 1.318170 0.902095
#[15] 0.600817 0.119643 0.345684 1.679935 2.539762 0.196152

# Diagrama de caja que relaciona el género con la actividad física y
# facetada por tipo de obesidad
ggplot(obesity, aes(x = Gender, y = actividad_fisica)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y género y resumir 
# por índice de masa corporal y actividad física
obesity_fisica <- obesity |>
  group_by(tipo_obesidad, Gender) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_fisica = median(actividad_fisica, na.rm = TRUE),
    .groups = "keep"
  )
obesity_fisica # produce:
# A tibble: 12 × 4
# Groups:   tipo_obesidad, Gender [12]
#  tipo_obesidad    Gender med_imc med_fisica
#  <fct>            <fct>    <dbl>      <dbl>
#1 Desnutricion     Male      17.6      1.99 
#2 Desnutricion     Female    17.5      1.19 
#3 Peso normal      Male      22.5      1    
#4 Peso normal      Female    22.0      1    
#5 Sobrepeso        Male      27.2      1    
#6 Sobrepeso        Female    26.7      0.719
#7 Obesidad grado 1 Male      32.7      1    
#8 Obesidad grado 1 Female    31.9      0.396
#9 Obesidad grado 2 Male      36.5      0.988
#10 Obesidad grado 2 Female    39.0      0.129
#11 Obesidad grado 3 Male      49.5      2    
#12 Obesidad grado 3 Female    42.8      0.617

# Diagrama de dispersión que relaciona el índice de masa corporal con
# la actividad física, coloreado por género y facetado por tipo de 
# obesidad
ggplot(obesity_fisica, aes(x = med_imc, y = med_fisica)) +
  geom_point(aes(color = Gender)) +
  facet_wrap(~tipo_obesidad)
