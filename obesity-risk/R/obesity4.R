library(tidyverse)
source("r4ds-progreso/obesity-risk/R/load_data.R")
#View(obesity)
getwd()

# Imprimir el obesity-risk data set
obesity # produce:
# A tibble: 20,758 × 20
#     id Gender   Age Height Weight family_history_with_overweight
#  <dbl> <chr>  <dbl>  <dbl>  <dbl>                          <dbl>
#1     0 Male    24.4   1.70   81.7                              1
#2     1 Female  18     1.56   57                                1
#3     2 Female  18     1.71   50.2                              1
#4     3 Female  21.0   1.71  131.                               1

# glimpse() sirve para mostrar todas las columnas
glimpse(obesity) # produce:
#Rows: 20,758
#Columns: 20
#$ id                              <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10…
#$ Gender                          <chr> "Male", "Female", "Female", "Fem…
#$ Age                             <dbl> 24.44301, 18.00000, 18.00000, 20…
#$ Height                          <dbl> 1.699998, 1.560000, 1.711460, 1.…
#$ Weight                          <dbl> 81.66995, 57.00000, 50.16575, 13…
#$ family_history_with_overweight  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,…
#$ frequent_high_calorie_food      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…

# Filtrar por género Male
obesity |>
  filter(Gender == "Male")
# Filtrar por género Female
obesity |>
  filter(Gender == "Female")

# Contar tipo de obesidad por género ordenado en orden descendente
obesity |>
  count(Gender, tipo_obesidad, sort = TRUE)
# Contar tipo de obesidad por género
obesity |>
  count(Gender, tipo_obesidad) # produce:
# A tibble: 12 × 3
#  Gender tipo_obesidad        n
#  <chr>  <fct>            <int>
#1 Female Desnutrición      1532
#2 Female Peso normal       1886
#3 Female Sobrepeso         1749
#4 Female Obesidad grado 1  1195
#5 Female Obesidad grado 2   914
#6 Female Obesidad grado 3  3146
#7 Male   Desnutrición       876
#8 Male   Peso normal       1640
#9 Male   Sobrepeso         2991
#10 Male   Obesidad grado 1  1923
#11 Male   Obesidad grado 2  2799
#12 Male   Obesidad grado 3   107

# Graficar un diagrama de dispersión que relaciona imc con actividad 
# física y género
ggplot(
  data = obesity,
  mapping = aes(x = physical_activity_frequency, y = imc, 
                color = Gender)
) +
  geom_point() + 
  geom_smooth(method = "lm")

# Seleccionar Gender, imc, Age y tipo_obesidad
obesity |>
  select(Gender, Age, imc, tipo_obesidad)

obesity_by_gender_type_mean_age_imc <- obesity |> 
  group_by(Gender, tipo_obesidad) |> 
  summarize(
    avg_imc = mean(imc, na.rm = TRUE),
    avg_age = mean(Age, na.rm = TRUE),
    .groups = "drop" # importante para que no salga el mensaje de que
    # se está agrupando obesity
  )
obesity_by_gender_type_mean_age_imc # produce:
# A tibble: 12 × 4
# Groups:   Gender [2]
#  Gender tipo_obesidad    avg_imc avg_age
#  <chr>  <fct>              <dbl>   <dbl>
#1 Female Desnutrición        17.3    19.7
#2 Female Peso normal         21.8    20.9
#3 Female Sobrepeso           27.1    24.9
#4 Female Obesidad grado 1    32.0    27.6
#5 Female Obesidad grado 2    38.5    26.0
#6 Female Obesidad grado 3    42.8    23.7
#7 Male   Desnutrición        17.2    18.7
#8 Male   Peso normal         22.4    21.1
#9 Male   Sobrepeso           27.3    24.2
#10 Male   Obesidad grado 1    32.8    24.3
#11 Male   Obesidad grado 2    37.0    27.6
#12 Male   Obesidad grado 3    41.7    27.4

obesity_by_gender_type_mean_age_imc 


obesity |> 
  group_by(Gender, tipo_obesidad) |> 
  summarize(
    n_imc = n()
  ) # produce:
# A tibble: 12 × 3
# Groups:   Gender [2]
#  Gender tipo_obesidad    n_imc
#  <chr>  <fct>            <int>
#1 Female Desnutrición      1532
#2 Female Peso normal       1886
#3 Female Sobrepeso         1749
#4 Female Obesidad grado 1  1195
#5 Female Obesidad grado 2   914
#6 Female Obesidad grado 3  3146
#7 Male   Desnutrición       876
#8 Male   Peso normal       1640
#9 Male   Sobrepeso         2991
#10 Male   Obesidad grado 1  1923
#11 Male   Obesidad grado 2  2799
#12 Male   Obesidad grado 3   107

# Conocer la cantidad de personas con obesidad grado 3 según su
# genero
obesity |>
  filter(tipo_obesidad == "Obesidad grado 3") |>
  group_by(Gender) |>
  summarize(
    n_imc = n()
  ) # produce:
# A tibble: 2 × 2
#  Gender n_imc
#  <chr>  <int>
#1 Female  3146
#2 Male     107

# Conocer la cantidad de personas por género
obesity |> 
  group_by(Gender) |>
  summarize(
    n_gender = n()
  ) # produce:
# A tibble: 2 × 2
#  Gender n_gender
#  <chr>     <int>
#1 Female    10422
#2 Male      10336



