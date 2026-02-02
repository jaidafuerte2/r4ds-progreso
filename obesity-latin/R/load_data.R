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
      imc < 18.5 ~ "Desnutricion",
      imc < 25 ~ "Peso normal",
      imc < 30 ~ "Sobrepeso",
      imc < 35 ~ "Obesidad grado 1",
      imc < 40 ~ "Obesidad grado 2",
      imc >= 40 ~ "Obesidad grado 3",
      TRUE ~ NA_character_
    )
  )

# Renombrar a las varibles de la data set de obesidad para que sean más
# legibles en español
obesity <- obesity |>
  rename(
    tiene_familiares_obesos = family_history_with_overweight,
    come_comida_muy_calorica = FAVC,
    come_vegetales = FCVC,
    comidas_principales = NCP,
    come_refrigerios = CAEC,
    fuma = SMOKE,
    agua_diaria = CH2O,
    toma_bebidas_calóricas = SCC,
    actividad_fisica = FAF,
    uso_tecnologia = TUE,
    consume_alcohol = CALC,
    medio_transporte = MTRANS
  )
# NOTA: Es mejor que las variables numéricas no tengan verbo, las 
# variables dicotomicas es mejor nombrarlas al inicio con verbos y
# las categorías a veces es bueno iniciarlas con verbos como en 
# consumo de alcohol, pero esto no siempre es ideal como en medio
# de transporte


# Conocer el tipo de la variable tipo de obesidad:
#class(obesity$tipo_obesidad) # produce: "character"
# Conocer el orden del tipo de valores de la variable tipo de obesidad
#unique(obesity$tipo_obesidad) # produce:
#[1] "Peso normal"      "Sobrepeso"       "Obesidad grado 1"
#[4] "Desnutricion"     "Obesidad grado 2" "Obesidad grado 3"
# Convertir la variable tipo de obesidad de character a factor:
obesity <- obesity |>
  mutate(
    tipo_obesidad = factor(
      tipo_obesidad,
      levels = c(
        "Desnutricion",
        "Peso normal",
        "Sobrepeso",
        "Obesidad grado 1",
        "Obesidad grado 2",
        "Obesidad grado 3"
      )
    )
  )
#class(obesity$tipo_obesidad) # produce: factor
#unique(obesity$tipo_obesidad) # produce:
#[1] Peso normal      Sobrepeso        Obesidad grado 1 Desnutricion    
#[5] Obesidad grado 2 Obesidad grado 3

# Convertir género a factor
obesity <- obesity |>
  mutate(
    Gender = factor(
      Gender,
      levels = c("Male", "Female")
    )
  )
#class(obesity$Gender) # produce: "factor"

# Traducir los valores de la variable de historia familiar de obesidad 
obesity <- obesity |>
  mutate(
    tiene_familiares_obesos = recode(
      tiene_familiares_obesos,
      "yes" = "si",
      "no" = "no"
    )
  )
#class(obesity$tiene_familiares_obesos) # produce: "character"

# Cambiar el tipo de la variable de historia familiar de obesidad
#a factor
obesity <- obesity |>
  mutate(
    tiene_familiares_obesos = factor(
      tiene_familiares_obesos,
      levels = c("si", "no")
    )
  )
#class(obesity$tiene_familiares_obesos) # produce: "factor"

# Traducir los valores de la variable comida muy calórica
obesity <- obesity |>
  mutate(
    come_comida_muy_calorica = recode(
      come_comida_muy_calorica,
      "yes" = "si",
      "no" = "no"
    )
  )
#class(obesity$come_comida_muy_calorica) # produce: "character"

# Cambiar el tipo de la variable comida muy calórica a factor
obesity <- obesity |>
  mutate(
    come_comida_muy_calorica = factor(
      come_comida_muy_calorica,
      levels = c("si", "no")
    )
  )
#class(obesity$come_comida_muy_calorica) # produce: "factor"

# Conocer los valores de la variable come vegetales
#unique(obesity$come_vegetales) # produce:
#[1] 2.000000 3.000000 1.000000 2.450218 2.880161 2.008760 2.596579
#[8] 2.591439 2.392665 1.123939 2.027574 2.658112 2.886260 2.714447
#[15] 2.750715 1.492500 2.205439 2.059138 2.310423 2.823179 2.052932
#[22] 2.596364 2.767731 2.815157 2.737762 2.568063 2.524428 2.971574
# Conocer el tipo de la variable come vegetales
#class(obesity$come_vegetales) # produce : "numeric"
# NOTA: Es posible que sean valores por semana, el manual no lo 
# indica. "numeric" es double o de doble precisión es decir que puede
# tener decimales

# Conocer los valores de la variable comidas principales
#unique(obesity$comidas_principales) # produce:
#[1] 2.000000 3.000000 1.000000 2.450218 2.880161 2.008760 2.596579
#[8] 2.591439 2.392665 1.123939 2.027574 2.658112 2.886260 2.714447
#[15] 2.750715 1.492500 2.205439 2.059138 2.310423 2.823179 2.052932
#[22] 2.596364 2.767731 2.815157 2.737762 2.568063 2.524428 2.971574
# Conocer el tipo de la varaible comidas principales
#class(obesity$comidas_principales) # produce: "numeric"

# Conocer los valores que tiene la variable come refrigerios
#unique(obesity$come_refrigerios) # produce: 
#[1] "Sometimes"  "Frequently" "Always"     "no"   
# Traducir los valores de la variable come refrigerios 
obesity <- obesity |>
  mutate(
    come_refrigerios = recode(
      come_refrigerios,
      "Sometimes" = "ocasional",
      "Frequently" = "frecuente",
      "no" = "no",
      "Always" = "siempre"
    )
  )
#unique(obesity$come_refrigerios) # produce:
#[1] "ocasional" "frecuente" "siempre"   "no"
#class(obesity$come_refrigerios) # produce: "character"

# Cambiar el tipo de la variable come refrigerios
obesity <- obesity |>
  mutate(
    come_refrigerios = factor(
      come_refrigerios, 
      levels = c("ocasional", "frecuente", "nunca", "siempre")
    )
  )
#class(obesity$come_refrigerios) # produce: "factor
