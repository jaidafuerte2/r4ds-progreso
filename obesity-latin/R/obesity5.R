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

# Agrupar la tabla obesidad por tipo de obesidad y resumir por los
# que si tienen familiares obesos.
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_familiares = sum(tiene_familiares_obesos == "si", na.rm = TRUE),
    perc_familiares = mean(tiene_familiares_obesos == "si",
                           na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_familiares perc_familiares med_age
#  <fct>                     <int>           <dbl>   <dbl>
#1 Desnutricion                124            45.8    19.2
#2 Peso normal                 163            54.3    21  
#3 Sobrepeso                   473            83.6    22.7
#4 Obesidad grado 1            361            98.1    23  
#5 Obesidad grado 2            337            99.7    26.0
#6 Obesidad grado 3            268           100      22.8

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

# Agrupar la tabla obesidad por tipo de obesidad y resumir por los que
# sí comen comida muy calórica
obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    sum_muy_calorica = sum(come_comida_muy_calorica == "si",
                            na.rm = TRUE),
    perc_muy_calorica = mean(come_comida_muy_calorica == "si",
                             na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_muy_calorica perc_muy_calorica med_age
#  <fct>                       <int>             <dbl>   <dbl>
#1 Desnutricion                  219              80.8    19.2
#2 Peso normal                   222              74      21  
#3 Sobrepeso                     473              83.6    22.7
#4 Obesidad grado 1              354              96.2    23  
#5 Obesidad grado 2              331              97.9    26.0
#6 Obesidad grado 3              267              99.6    22.8

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
    avg_vegetales = mean(come_vegetales, na.rm = TRUE),
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

# Agrupar por tipo de obesidad y resumir por edad y los que comen 
# siempre vegetales
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_vegetales_igual3 = sum(come_vegetales == 3, na.rm = TRUE),
    perc_vegetales_igual3 = mean(come_vegetales == 3,
                                 na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_vegetales_igual3 perc_vegetales_igual3 med_age
#  <fct>                           <int>                 <dbl>   <dbl>
#1 Desnutricion                       81                 29.9     19.2
#2 Peso normal                       117                 39       21  
#3 Sobrepeso                          75                 13.3     22.7
#4 Obesidad grado 1                   28                  7.61    23  
#5 Obesidad grado 2                   83                 24.6     26.0
#6 Obesidad grado 3                  268                100       22.8

# Agrupar por tipo de obesidad y resumir por edad y los que nunca comen 
# vegetales
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_vegetales_igual1 = sum(come_vegetales == 1, na.rm = TRUE),
    perc_vegetales_igual1 = mean(come_vegetales == 1,
                                 na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:

# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_vegetales_igual1 perc_vegetales_igual1 med_age
#  <fct>                           <int>                 <dbl>   <dbl>
#1 Desnutricion                        4                 1.48     19.2
#2 Peso normal                        18                 6        21  
#3 Sobrepeso                           7                 1.24     22.7
#4 Obesidad grado 1                    3                 0.815    23  
#5 Obesidad grado 2                    1                 0.296    26.0
#6 Obesidad grado 3                    0                 0        22.8

################################################
##
## 4.- Número de Comidas Principales y edad
##
################################################

# Conocer el tipo de la varaible de comidas principales
class(obesity$comidas_principales) # produce: "numeric"

# Conocer los valores de la varaible comidas principales
unique(obesity$comidas_principales)[1:20] # produce:
#[1] 3.000000 1.000000 4.000000 3.289260 3.995147 1.726260 2.581015
#[8] 1.600812 1.737620 1.105480 2.084600 1.894384 2.857787 3.765526
#[15] 3.285167 3.691226 3.156153 1.079760 3.559841 3.891994

# Diagrama de dispersión que relaciona la edad con las comidas principales
# coloreado, y facetado por tipo de obesidad
ggplot(obesity, aes(x = Age, y = comidas_principales)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y resumir por número
# de comidas y edad
obesity_comidas <- obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    med_comidas = median(comidas_principales, na.rm = TRUE),
    avg_comidas = mean(comidas_principales, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    n_tipo = n(),
    .groups = "keep"
  )
obesity_comidas # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    med_comidas avg_comidas med_age
#  <fct>                  <dbl>       <dbl>   <dbl>
#1 Desnutricion               3        2.91    19.2
#2 Peso normal                3        2.73    21  
#3 Sobrepeso                  3        2.50    22.7
#4 Obesidad grado 1           3        2.45    23  
#5 Obesidad grado 2           3        2.79    26.0
#6 Obesidad grado 3           3        3       22.8

# Diagrama de dispersión que relaciona la edad con el número de comidas
# coloreado y facetado por tipo de obesidad
ggplot(obesity_comidas, aes(x = med_age, y = avg_comidas)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por edad
# y los que comen menos de 3 veces al días
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_comidas_menos3 = mean(comidas_principales < 3, na.rm = TRUE),
    perc_comidas_menos3 = mean(comidas_principales < 3
                               , na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_comidas_menos3 perc_comidas_menos3 med_age
#  <fct>                         <dbl>               <dbl>   <dbl>
#1 Desnutricion                  0.247                24.7    19.2
#2 Peso normal                   0.203                20.3    21  
#3 Sobrepeso                     0.481                48.1    22.7
#4 Obesidad grado 1              0.486                48.6    23  
#5 Obesidad grado 2              0.299                29.9    26.0
#6 Obesidad grado 3              0                     0      22.8

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por edad
# y los que comen menos de 2 veces al días
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_comidas_menos2 = mean(comidas_principales <= 2, na.rm = TRUE),
    perc_comidas_menos2 = mean(comidas_principales <= 2
                               , na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_comidas_menos2 perc_comidas_menos2 med_age
#  <fct>                         <dbl>               <dbl>   <dbl>
#1 Desnutricion                 0.181                18.1     19.2
#2 Peso normal                  0.187                18.7     21  
#3 Sobrepeso                    0.281                28.1     22.7
#4 Obesidad grado 1             0.269                26.9     23  
#5 Obesidad grado 2             0.0947                9.47    26.0
#6 Obesidad grado 3             0                     0       22.8

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por edad
# y los que comen mas de 3 veces al días
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_comidas_mas3 = mean(comidas_principales > 3, na.rm = TRUE),
    perc_comidas_mas3 = mean(comidas_principales > 3
                               , na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_comidas_mas3 perc_comidas_mas3 med_age
#  <fct>                       <dbl>             <dbl>   <dbl>
#1 Desnutricion              0.402              40.2      19.2
#2 Peso normal               0.103              10.3      21  
#3 Sobrepeso                 0.129              12.9      22.7
#4 Obesidad grado 1          0.00815             0.815    23  
#5 Obesidad grado 2          0.0355              3.55     26.0
#6 Obesidad grado 3          0                   0        22.8

# Agrupar la tabla de obesidad por tipo de obesidad y resumir por edad
# y los que comen mas de 4 veces al días
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_comidas_mas4 = mean(comidas_principales >= 4, na.rm = TRUE),
    perc_comidas_mas4 = mean(comidas_principales >= 4
                             , na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_comidas_mas4 perc_comidas_mas4 med_age
#  <fct>                       <dbl>             <dbl>   <dbl>
#1 Desnutricion              0.103              10.3      19.2
#2 Peso normal               0.1                10        21  
#3 Sobrepeso                 0.0177              1.77     22.7
#4 Obesidad grado 1          0                   0        23  
#5 Obesidad grado 2          0.00296             0.296    26.0
#6 Obesidad grado 3          0                   0        22.8

#####################################
##
## 5.- Consumo de Refrigerios y edad
##
#####################################

# Conocer el tipo de la variable consumo de refrigerios
class(obesity$come_refrigerios) # produce:  "factor"

# Conocer los valores de la varaible consumo de refrigerio
unique(obesity$come_refrigerios) # produce : 
# [1] ocasional frecuente siempre   nunca 

# Diagrama de caja que relaciona el consumo de refrigerios con la edad,
# coloreado por consumo de refrigerios y facetado por tipo de obesidad
ggplot(obesity, aes(x = come_refrigerios, y = Age)) +
  geom_boxplot(aes(color = come_refrigerios)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por consumo de refrigerios y resumir por
# índice de masa corporal y edad
obesity_refrigerios <- obesity |> 
  group_by(come_refrigerios) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_refrigerios # produce:
# A tibble: 4 × 3
# Groups:   come_refrigerios [4]
#  come_refrigerios med_imc med_age
#  <fct>              <dbl>   <dbl>
#1 ocasional           31.2      23
#2 frecuente           18.9      21
#3 nunca               26.4      21
#4 siempre             23.8      21

# Diagrama de dispersión que relaciona la edad con el índice de masa 
# corporal, coloreado por consumo de refrigerios
ggplot(obesity_refrigerios, aes(x = med_age, y = med_imc)) +
  geom_point(aes(color = come_refrigerios))

# Agrupar tabla por tipo de obesidad y resumir por los que nunca
# comen refrigerios y su edad
obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    sum_numca = sum(come_refrigerios == "nunca", na.rm = TRUE),
    perc_nunca = mean(come_refrigerios == "nunca", 
                      na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_numca perc_nunca med_age
#  <fct>                <int>      <dbl>   <dbl>
#1 Desnutricion             3      1.11     19.2
#2 Peso normal             10      3.33     21  
#3 Sobrepeso               36      6.36     22.7
#4 Obesidad grado 1         1      0.272    23  
#5 Obesidad grado 2         1      0.296    26.0
#6 Obesidad grado 3         0      0        22.8

# Agrupar tabla por tipo de obesidad y resumir por los que siempre
# comen refrigerios y su edad
obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    sum_siempre = sum(come_refrigerios == "siempre", na.rm = TRUE),
    perc_siempre = mean(come_refrigerios == "siempre", 
                      na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_siempre perc_siempre med_age
#  <fct>                  <int>        <dbl>   <dbl>
#1 Desnutricion               2        0.738    19.2
#2 Peso normal               35       11.7      21  
#3 Sobrepeso                  8        1.41     22.7
#4 Obesidad grado 1           6        1.63     23  
#5 Obesidad grado 2           2        0.592    26.0
#6 Obesidad grado 3           0        0        22.8


#####################################
##
## 6.- Hábito de Fumar y Género
##
#####################################

# Conocer el tipo de la variable de hábito de fumar
class(obesity$fuma) # produce: "factor"

# Conocer los valores de la variable hábito de fumar
unique(obesity$fuma) # produce: [1] no si

# Diagrama de caja que relaciona el hábito de fumar con la edad, facetado
# por tipo de obesidad
ggplot(obesity, aes(x = fuma, y = Age)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

obesity_fuma <- obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    fumadores = sum(fuma == "si", na.rm = TRUE),
    prop_fumadores = mean(fuma == "si", na.rm = TRUE) * 100,
    #round(mean(SMOKE == "yes") * 100, 1) # formato bonito
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_fuma # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    fumadores prop_fumadores med_age
#  <fct>                <int>          <dbl>   <dbl>
#1 Desnutricion             1          0.369    19.2
#2 Peso normal             13          4.33     21  
#3 Sobrepeso                8          1.41     22.7
#4 Obesidad grado 1         6          1.63     23  
#5 Obesidad grado 2        15          4.44     26.0
#6 Obesidad grado 3         1          0.373    22.8

ggplot(obesity_fuma, aes(x = med_age, y = prop_fumadores)) +
  geom_point(aes(color = tipo_obesidad))

##################################
##
## 7.- Consumo de Agua y Edad
##
##################################  

# Conocer el tipo de la variable de consumo de agua 
class(obesity$agua_diaria) # produce: "numeric"

# Conocer el tipo de valores de la variable de consumo de agua
unique(obesity$agua_diaria)[1:20] # produce:
#[1] 2.000000 3.000000 1.000000 1.152736 1.115967 2.704507 2.184707
#[8] 2.406541 2.984323 2.444125 2.654702 2.825629 2.847264 2.884033
#[15] 2.147746 2.815293 2.593459 1.031354 2.651258 1.792022

# Diagrama de dispersión que relaciona la edad con el consumo de agua,
# facetado por tipo de obesidad
ggplot(obesity, aes(x = Age, y = agua_diaria)) +
  geom_point() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y resumir por edad y
# consumo de agua
obesity_agua <- obesity |> 
  group_by(tipo_obesidad) |>
  summarise(
    med_agua = median(agua_diaria, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_agua # produce:  
# A tibble: 6 × 3
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    med_agua med_age
#  <fct>               <dbl>   <dbl>
#1 Desnutricion         2       19.2
#2 Peso normal          2       21  
#3 Sobrepeso            2       22.7
#4 Obesidad grado 1     2.02    23  
#5 Obesidad grado 2     2       26.0
#6 Obesidad grado 3     2.50    22.8

# Diagrama de dispersión que relaciona la edad con el consumo de agua,
# coloreado por tipo de obesidad
ggplot(obesity_agua, aes(x = med_age, y = med_agua)) +
  geom_point(aes(color = tipo_obesidad))

# Agrupar la tabla obesidad por tipo de obesidad y resumir por quienes
# consumen más de 2 litros de agua
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_agua2 = sum(agua_diaria > 2, na.rm = TRUE),
    prop_agua2 = mean(agua_diaria > 2, na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_agua2 prop_agua2 med_age
#  <fct>                <int>      <dbl>   <dbl>
#1 Desnutricion            90       33.2    19.2
#2 Peso normal             43       14.3    21  
#3 Sobrepeso              224       39.6    22.7
#4 Obesidad grado 1       186       50.5    23  
#5 Obesidad grado 2       160       47.3    26.0
#6 Obesidad grado 3       191       71.3    22.8

###############################################
##
## 8.- Consumo de bebidas calóricas y edad
##
###############################################

# Conocer el tipo de la variable conusmo de bebidas calóricas
class(obesity$toma_bebidas_calóricas) # produce: "factor"

# Conocer los valores de la variable consumo de bebidas calóricas
unique(obesity$toma_bebidas_calóricas) # produce: [1] no si

# Diagrama de caja que relaciona el consumo de bebidas calóricas con 
# la edad, facetado por tipo de obesidad
ggplot(obesity, aes(x = toma_bebidas_calóricas, y = Age)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

obesity |>
  group_by(toma_bebidas_calóricas) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 2 × 3
# Groups:   toma_bebidas_calóricas [2]
#  toma_bebidas_calóricas med_imc med_age
#  <fct>                    <dbl>   <dbl>
#1 no                        29.3    22.9
#2 si                        24.2    19.0

obesity_caloricas <- obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_caloricas = sum(toma_bebidas_calóricas == "si", na.rm = TRUE),
    prop_caloricas = mean(toma_bebidas_calóricas == "si", 
                          na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_caloricas # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_caloricas prop_caloricas med_age
#  <fct>                    <int>          <dbl>   <dbl>
#1 Desnutricion                22          8.12     19.2
#2 Peso normal                 37         12.3      21  
#3 Sobrepeso                   34          6.01     22.7
#4 Obesidad grado 1             2          0.543    23  
#5 Obesidad grado 2             1          0.296    26.0
#6 Obesidad grado 3             0          0        22.8

# Diagrama de dispersión que relaciona la edad con el porcentaje de
# personas que sí toman bebidas calóricas
ggplot(obesity_caloricas, aes(x = med_age, y = prop_caloricas)) +
  geom_point(aes(color = tipo_obesidad))

####################################
##
## 9.- Actividad Física y Edad
##
####################################

# Conocer el tipo de la variable de actividad física
class(obesity$actividad_fisica) # produce: "numeric"

# Conocer los valores de la variable actividad física
unique(obesity$actividad_fisica)[1:20] # produce: 
#[1] 0.000000 3.000000 2.000000 1.000000 0.319156 1.541072 1.978631
#[8] 0.100320 1.586525 1.399183 1.680844 2.206738 1.318170 0.902095
#[15] 0.600817 0.119643 0.345684 1.679935 2.539762 0.196152

# Diagrama de dispersión que relaciona el edad con la actividad física,
# coloreado y facetado por tipo de obesidad
ggplot(obesity, aes(x = Age, y = actividad_fisica)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y resumir por actividad
# física y edad
obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    med_actividad = median(actividad_fisica, na.rm = TRUE),
    avg_actividad = mean(actividad_fisica, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    med_actividad avg_actividad med_age
#  <fct>                    <dbl>         <dbl>   <dbl>
#1 Desnutricion             1.35          1.25     19.2
#2 Peso normal              1             1.22     21  
#3 Sobrepeso                1             1.01     22.7
#4 Obesidad grado 1         1             1.01     23  
#5 Obesidad grado 2         0.859         0.826    26.0
#6 Obesidad grado 3         0.622         0.774    22.8

obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    sum_actividad_equal0 = sum(actividad_fisica == 0, na.rm = TRUE),
    perc_actividad_equal0 = mean(actividad_fisica == 0,
                                 na.rm = TRUE) * 100,
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  ) # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    sum_actividad_equal0 perc_actividad_equal0 med_age
#  <fct>                           <int>                 <dbl>   <dbl>
#1 Desnutricion                       35                 12.9     19.2
#2 Peso normal                        81                 27       21  
#3 Sobrepeso                          99                 17.5     22.7
#4 Obesidad grado 1                   80                 21.7     23  
#5 Obesidad grado 2                   33                  9.76    26.0
#6 Obesidad grado 3                   83                 31.0     22.8
# NOTA: Esto es muy interesante porque las personas de peso normal
# hacen menos ejercicio que las personas con obesidad grado 1 y grado 2

#######################################
##
## 10.- Uso de Tecnología y edad
##
#######################################

# Conocer el tipo de la varaible de uso de tecnología
class(obesity$uso_tecnologia) # produce: "numeric"

# Conocer los valores de la variable tecnología
unique(obesity$uso_tecnologia)[1:20] # produce:
#[1] 1.000000 0.000000 2.000000 0.294990 0.838957 0.479221 0.625350
#[8] 0.265790 0.555468 0.928972 1.340107 0.589980 1.374650 1.283673
#[15] 0.062488 0.997400 0.738269 0.860497 0.478676 0.555967

# Diagrama de dispersión que relaciona la edad con el uso de 
# tecnología , coloreado y facetado por el tipo de obesidad
ggplot(obesity, aes(x = Age, y = uso_tecnologia)) +
  geom_point(aes(color = tipo_obesidad)) +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por tipo de obesidad y resumido por la
# variable uso de tecnología y edad
obesity_tecnologia <- obesity |>
  group_by(tipo_obesidad) |>
  summarise(
    med_tecnologia = median(uso_tecnologia, na.rm = TRUE),
    avg_tecnologia = mean(uso_tecnologia, na.rm = TRUE),
    med_age = median(Age, ne.rm = TRUE),
    .groups= "keep" 
  )
obesity_tecnologia # produce:
# A tibble: 6 × 4
# Groups:   tipo_obesidad [6]
#  tipo_obesidad    med_tecnologia avg_tecnologia med_age
#  <fct>                     <dbl>          <dbl>   <dbl>
#1 Desnutricion              1              0.846    19.2
#2 Peso normal               1              0.690    21  
#3 Sobrepeso                 0.553          0.648    22.7
#4 Obesidad grado 1          0.513          0.650    23  
#5 Obesidad grado 2          0.476          0.562    26.0
#6 Obesidad grado 3          0.649          0.586    22.8

# Diagrama de dispersión que relaciona la edad con el uso de tecnología
# y coloreado por tipo de obesidad
ggplot(obesity_tecnologia, aes(x = med_age, y = avg_tecnologia)) +
  geom_point(aes(color = tipo_obesidad))

#####################################
##
## 11.- Consumo de Alcohol y Edad
##
#####################################

# Conocer el tipo de la variable de consumo de alcohol
class(obesity$consume_alcohol) # produce: "factor"

# Conocer los valores de la variable de consumo de alcohol
unique(obesity$consume_alcohol) # produce: 
# [1] nunca     ocasional frecuente siempre 

# Diagrama de caja que relaciona el consumo de alcohol con la edad,
# facetado por tipo de obesidad
ggplot(obesity, aes(x = consume_alcohol, y = Age)) + 
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# Agrupar la tabla obesidad por consumo de alcohol y resumir por índice
# de masa corporal y edad
obesity_alcohol <- obesity |> 
  group_by(consume_alcohol) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    med_age = median(Age, na.rm = TRUE),
    .groups = "keep"
  )
obesity_alcohol # produce:
# A tibble: 4 × 4
# Groups:   consume_alcohol [4]
#  consume_alcohol med_imc avg_imc med_age
#  <fct>             <dbl>   <dbl>   <dbl>
#1 nunca              28.0    27.1      22
#2 ocasional          30.9    31.0      23
#3 frecuente          26.9    27.0      24
#4 siempre            22.5    22.5      21

# Diagrama de dispersión que relaciona la edad con el índice de masa 
# corporal, coloreado y formado por consumo de alcohol 
ggplot(obesity_alcohol, aes(x = med_age, y = med_imc)) + 
  geom_point(aes(color = consume_alcohol, shape = consume_alcohol))

#########################################
##
## 12.- Medio de Transporte y Edad
##
#########################################

# Conocer el tipo de la variable medio de transporte
class(obesity$medio_transporte) # produce: "factor"

# Conocer los valores de la variable medio de transporte
unique(obesity$medio_transporte) # produce:
#[1] transporte_publico camina             auto              
#[4] moto               bicicleta 

# Diagrama de caja que relaciona la variable de medio transporte con
# con la edad y facetado por tipo de obesidad
ggplot(obesity, aes(x = medio_transporte, y = Age)) +
  geom_boxplot() +
  facet_wrap(~tipo_obesidad)

# 
obesity_transporte <- obesity |>
  group_by(medio_transporte) |>
  summarise(
    med_imc = median(imc, na.rm = TRUE),
    avg_imc = mean(imc, na.rm = TRUE),
    med_age = median(imc, na.rm = TRUE),
    .groups = "keep"
  ) 
obesity_transporte # produce:
# A tibble: 5 × 4
# Groups:   medio_transporte [5]
#  medio_transporte   med_imc avg_imc med_age
#  <fct>                <dbl>   <dbl>   <dbl>
#1 transporte_publico    29.0    30.1    29.0
#2 auto                  29.0    29.2    29.0
#3 moto                  24.4    25.8    24.4
#4 camina                23.6    23.7    23.6
#5 bicicleta             24.2    25.2    24.2
# NOTA: los únicos con un peso adecuado en el promedio son los que 
# caminan

# Diagrama de dispersión que relaciona la edad con el índice de masa 
# corporal, coloreado y formado por medio de transporte
ggplot(obesity_transporte, aes(x = med_age, y = med_imc)) +
  geom_point(aes(color = medio_transporte, shape = medio_transporte))
