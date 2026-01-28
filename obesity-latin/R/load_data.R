library(readr)
library(dplyr)
library(tidyverse)

# Importar desde el IDE la tabla obesity latin y cuidar de llamarla 
# obesity

# Crear la variable imc y calcular el índice de masa corporal.
obesity <- obesity |>
  mutate(
    imc = Weight / (Height ^ 2)
  )

# Reclasificar el tipo de obesidad porque no coincide con la 
# clasificación de la OMS
obesity <- obesity |>
  mutate(
    tipo_obesidad = case_when(
      imc < 18.5 ~ "Desnutrición",
      imc < 25 ~ "Peso normal",
      imc < 30 ~ "Sobrepesso",
      imc < 35 ~ "Obesidad grado 1",
      imc < 40 ~ "Obesidad grado 2",
      imc >= 40 ~ "Obesidad grado 3",
      TRUE ~ NA_character_
    )
  )
#View(obesity)
