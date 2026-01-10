library(readr)
library(dplyr)
library(tidyverse)

# Abrir la base de datos con readr.read_csv() 
#obesity <- read_csv("r4ds-progreso/obesity-risk/obesity_level.csv")
#View(obesity)

# Crear la variable imc y calcular el índice de masa corporal con
# dply.mutate()
obesity <- obesity %>%
  mutate(
    imc = Weight / (Height ^ 2 )
  )
#View(obesity)

# Reclasificar el tipo de obesidad pues no coincide con la clasificación
# de la OMS
obesity <- obesity %>%
  mutate(
    tipo_obesidad = case_when(
      imc < 18.5 ~ "Desnutrición",
      imc < 25   ~ "Peso normal",
      imc < 30   ~ "Sobrepeso",
      imc < 35   ~ "Obesidad grado 1",
      imc < 40   ~ "Obesidad grado 2",
      imc >= 40  ~ "Obesidad grado 3",
      TRUE       ~ NA_character_
    )
  )

# Conteo por categoría:
count(obesity, tipo_obesidad) # produce:
# A tibble: 6 × 2
#  tipo_obesidad        n
#  <chr>            <int>
#1 Desnutrición      2408
#2 Obesidad grado 1  3118
#3 Obesidad grado 2  3713
#4 Obesidad grado 3  3253
#5 Peso normal       3526
#6 Sobrepeso         4740

# Covertir tipo_obesidad en factor:
obesity <- obesity %>%
  mutate(
    tipo_obesidad = factor(
      tipo_obesidad,
      levels = c(
        "Desnutrición",
        "Peso normal",
        "Sobrepeso",
        "Obesidad grado 1",
        "Obesidad grado 2",
        "Obesidad grado 3"
      )
    )
  )

obesity <- obesity %>%
  rename(
    frequent_high_calorie_food      = FAVC,
    vegetable_consumption_frequency = FCVC,
    number_main_meals               = NCP,
    food_between_meals              = CAEC,
    daily_water_consumption         = CH2O,
    caloric_beverages_consumption   = SCC,
    physical_activity_frequency     = FAF,
    technology_use_time             = TUE,
    alcohol_consumption             = CALC,
    transportation_mode             = MTRANS
  )

# Cambiar la variable family_history_with_overweight de double a factor
# y cambiar los valores de número a categorías
obesity <- obesity %>%
  mutate(
    family_history_with_overweight = factor(
      family_history_with_overweight,
      levels = c(0, 1),
      labels = c("No", "Si")
    )
  )

obesity <- obesity %>%
  mutate(
    frequent_high_calorie_food = factor(
      frequent_high_calorie_food,
      levels = c(0, 1),
      labels = c("No", "Si")
    )
  )

# Covertir food_between_meals en factor:
obesity <- obesity %>%
  mutate(
    food_between_meals = factor(
      food_between_meals,
      levels = c(
        "Sometimes",
        "Frequently",
        "0",
        "Always"
      )
    )
  )

# Convertir a la variable SMOKE en un factor con categorías Si y No
obesity <- obesity %>%
  mutate(
    SMOKE = factor(
      SMOKE,
      levels = c(0, 1),
      labels = c("No", "Si")
    )
  )

# Convertir a la variable consumo de bebidas calóricas en un factor 
# con categorías Si y No
obesity <- obesity %>%
  mutate(
    caloric_beverages_consumption = factor(
      caloric_beverages_consumption,
      levels = c(0, 1),
      labels = c("No", "Si")
    )
  )
#View(obesity)

#write_csv(
#  obesity,
#  "obesity.csv"
#)
