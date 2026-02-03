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
unique(obesity$come_refrigerios) # produce: 
#[1] "Sometimes"  "Frequently" "Always"     "no"   
# Traducir los valores de la variable come refrigerios 
obesity <- obesity |>
  mutate(
    come_refrigerios = recode(
      come_refrigerios,
      "Sometimes" = "ocasional",
      "Frequently" = "frecuente",
      "no" = "nunca",
      "Always" = "siempre"
    )
  )
unique(obesity$come_refrigerios) # produce:
#[1] "ocasional" "frecuente" "siempre"   "nunca"
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

#unique(obesity$fuma) # produce: # [1] "no"  "yes"
# Traducir los valores de la variable fumar
obesity <- obesity |>
  mutate(
    fuma = recode(
      fuma,
      "yes" = "si",
      "no" = "no"
    )
  )
unique(obesity$fuma) # produce: # [1] "no" "si"

# Cambiar el tipo de la variable hábito de fumar a factor
obesity <- obesity |>
  mutate(
    fuma = factor(
      fuma,
      levels = c("si", "no")
    )
  )
class(obesity$fuma) # produce: "factor"

# Conocer los valores que tiene la variable concumo de agua diaria
unique(obesity$agua_diaria) # produce:
#[1] 2.000000 3.000000 1.000000 1.152736 1.115967 2.704507 2.184707
#[8] 2.406541 2.984323 2.444125 2.654702 2.825629 2.847264 2.884033
#[15] 2.147746 2.815293 2.593459 1.031354 2.651258 1.792022 1.490613
#[22] 2.074048 2.137550 2.456581 1.527036 1.322669 1.656082 1.569082

# Conocer el tipo de la variable de consumo de agua diaria
#class(obesity$agua_diaria) # produce: "numeric"

#unique(obesity$toma_bebidas_calóricas) # produce:
# [1] "no"  "yes"

# Traducir los valores de la variable consumo de bebidas calóricas
obesity <- obesity |>
  mutate(
    toma_bebidas_calóricas = recode(
      toma_bebidas_calóricas,
      "no" = "no",
      "yes" = "si"
    )
  )
#unique(obesity$toma_bebidas_calóricas) # produce: [1] no si

# Cambiar el tipo de la variable de consumo de bebidas calóricas
obesity <- obesity |>
  mutate(
    toma_bebidas_calóricas = factor(
      toma_bebidas_calóricas,
      levels = c("no", "si")
    )
  )
#class(obesity$toma_bebidas_calóricas) # produce: "factor"

#unique(obesity$actividad_fisica) # produce:
#[1] 0.000000 3.000000 2.000000 1.000000 0.319156 1.541072 1.978631
#[8] 0.100320 1.586525 1.399183 1.680844 2.206738 1.318170 0.902095
#[15] 0.600817 0.119643 0.345684 1.679935 2.539762 0.196152 1.596576
#[22] 0.754646 0.427770 0.545931 1.303976 1.488843 1.228136 1.661556

#class(obesity$actividad_fisica) # produce: "numeric"

# Conocer los valores de la varaible de uso de tecnología
#unique(obesity$uso_tecnologia) # produce:
#[1] 1.000000 0.000000 2.000000 0.294990 0.838957 0.479221 0.625350
#[8] 0.265790 0.555468 0.928972 1.340107 0.589980 1.374650 1.283673
#[15] 0.062488 0.997400 0.738269 0.860497 0.478676 0.555967 0.469735
#[22] 0.371941 0.009254 0.832400 0.114716 0.256382 1.547086 1.906141

# Conocer el tipo de la variable de uso de tecnnología
#class(obesity$uso_tecnologia) # produce: "numeric"

# Conocer los valores de la variable consumo de alcohol
#unique(obesity$consume_alcohol) # produce:
#[1] "no"         "Sometimes"  "Frequently" "Always"    

# Traducir los valores de la variable consumo de alcohol
obesity <- obesity |>
  mutate(
    consume_alcohol = recode(
      consume_alcohol,
      "no" = "nunca",
      "Sometimes" = "ocasional",
      "Frequently" = "frecuente",
      "Always" = "siempre"
    )
  )
#unique(obesity$consume_alcohol) # produce:
# [1] "nunca"     "ocasional" "frecuente" "siempre" 

# Cambiar el tipo de la variable de consumo de alcohol
obesity <- obesity |>
  mutate(
    consume_alcohol = factor(
      consume_alcohol,
      levels = c("nunca", "ocasional", "frecuente", "siempre")
    )
  )
#class(obesity$consume_alcohol) # produce: "factor"

#unique(obesity$medio_transporte) # produce:
#[1] "Public_Transportation" "Walking"              
#[3] "Automobile"            "Motorbike"            
#[5] "Bike"  

# Traducir los valores de la variable medio de transporte
obesity <- obesity |>
  mutate(
    medio_transporte = recode(
      medio_transporte,
      "Public_Transportation" = "transporte_publico",
      "Walking" = "camina",
      "Automobile" = "auto",
      "Motorbike" = "moto",
      "Bike" = "bicicleta"
    )
  )
#unique(obesity$medio_transporte) # produce:
#[1] "transporte_publico" "camina"             "auto"              
#[4] "moto"               "bicicleta"  

# Cambiar el tipo de la variable medio de transporte a factor
obesity <- obesity |>
  mutate(
    medio_transporte = factor(
      medio_transporte,
      levels = c("transporte_publico", "auto", "moto", "camina",
                 "bicicleta")
    )
  )
#class(obesity$medio_transporte) # produce: "factor"
