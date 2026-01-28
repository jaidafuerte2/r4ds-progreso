source("r4ds-progreso/obesity-latin/R/load_data.R")

# Imprimir el obesity-latin
obesity # produce:
# A tibble: 20,758 × 20
#     id Gender   Age Height Weight family_history_with_overweight
#  <dbl> <fct>  <dbl>  <dbl>  <dbl> <fct>                         
#1     0 Male    24.4   1.70   81.7 Si                            
#2     1 Female  18     1.56   57   Si                            
#3     2 Female  18     1.71   50.2 Si                            
#4     3 Female  21.0   1.71  131.  Si  

# Llamar a obesity como parámetro de la función glimpse para visulaizar
# las variables como filas (horizontalmente)
glimpse(obesity) # produce:
#Rows: 20,758
#Columns: 20
#$ id                              <dbl> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10…
#$ Gender                          <fct> Male, Female, Female, Female, Ma…
#$ Age                             <dbl> 24.44301, 18.00000, 18.00000, 20…
#$ Height                          <dbl> 1.699998, 1.560000, 1.711460, 1.…
#$ Weight                          <dbl> 81.66995, 57.00000, 50.16575, 13…
#$ family_history_with_overweight  <fct> Si, Si, Si, Si, Si, Si, Si, Si, …
#$ frequent_high_calorie_food      <fct> Si, Si, Si, Si, Si, Si, Si, Si, …
#$ vegetable_consumption_frequency <dbl> 2.000000, 2.000000, 1.880534, 3.…
#$ number_main_meals               <dbl> 2.983297, 3.000000, 1.411685, 3.…
#$ food_between_meals              <fct> Sometimes, Frequently, Sometimes…
#$ SMOKE                           <fct> No, No, No, No, No, No, No, No, …
#$ daily_water_consumption         <dbl> 2.763573, 2.000000, 1.910378, 1.…
#$ caloric_beverages_consumption   <fct> No, No, No, No, No, No, No, No, …
#$ physical_activity_frequency     <dbl> 0.000000, 1.000000, 0.866045, 1.…
#$ technology_use_time             <dbl> 0.976473, 1.000000, 1.673584, 0.…
#$ alcohol_consumption             <fct> Sometimes, 0, 0, Sometimes, Some…
#$ transportation_mode             <fct> Public_Transportation, Automobil…
#$ `0be1dad`                       <chr> "Overweight_Level_II", "0rmal_We…
#$ imc                             <dbl> 28.25956, 23.42209, 17.12671, 44…
#$ tipo_obesidad                   <chr> "Sobrepesso", "Peso normal", "De…

# Filtrar obesidad por género masculino
obesity |>
  filter(Gender == "Male") # produce:
# A tibble: 10,336 × 20
#     id Gender   Age Height Weight family_history_with_overweight
#  <dbl> <fct>  <dbl>  <dbl>  <dbl> <fct>                         
#1     0 Male    24.4   1.70   81.7 Si                            
#2     4 Male    31.6   1.91   93.8 Si                            
#3     5 Male    18.1   1.75   51.6 Si                            
#4     6 Male    29.9   1.75  113.  Si       

# Filtrar obesidad por género femenino
obesity |>
  filter(Gender == "Female") # produce:
# A tibble: 10,422 × 20
#     id Gender   Age Height Weight family_history_with_overweight
#  <dbl> <fct>  <dbl>  <dbl>  <dbl> <fct>                         
#1     1 Female  18     1.56   57   Si                            
#2     2 Female  18     1.71   50.2 Si                            
#3     3 Female  21.0   1.71  131.  Si                            
#4     9 Female  26     1.64  111.  Si  

# Contar por tipo de obesidad y género; y mostrar en orden 
# descendente
obesity |>
  count(Gender, tipo_obesidad, sort = TRUE) # produce:
# A tibble: 10 × 3
#  Gender tipo_obesidad        n
#  <fct>  <chr>            <int>
#1 Female obesidad grado 2  4060
#2 Male   Sobrepesso        2991
#3 Male   obesidad grado 2  2906
#4 Male   Obesidad grado 1  1923
#5 Female Peso normal       1886
#6 Female Sobrepesso        1749
#7 Male   Peso normal       1640
#8 Female Desnutrición      1532
#9 Female Obesidad grado 1  1195
#10 Male   Desnutrición       876

# Contar por tipo de obesidad y género
obesity |> 
  count(tipo_obesidad, Gender) # produce:
# A tibble: 10 × 3
#  tipo_obesidad    Gender     n
#  <chr>            <fct>  <int>
#1 Desnutrición     Male     876
#2 Desnutrición     Female  1532
#3 Obesidad grado 1 Male    1923
#4 Obesidad grado 1 Female  1195
#5 Peso normal      Male    1640
#6 Peso normal      Female  1886
#7 Sobrepesso       Male    2991
#8 Sobrepesso       Female  1749
#9 obesidad grado 2 Male    2906
#10 obesidad grado 2 Female  4060

# En la tabla de obesidad seleccionar por género, índice de masa 
# corporal, edad y tipo de obesidad
obesity |>
  select(Gender, imc, Age, tipo_obesidad)
# A tibble: 20,758 × 4
#  Gender   imc   Age tipo_obesidad   
#  <fct>  <dbl> <dbl> <chr>           
#1 Male    28.3  24.4 Sobrepesso      
#2 Female  23.4  18   Peso normal     
#3 Female  17.1  18   Desnutrición    
#4 Female  44.9  21.0 obesidad grado 2
#5 Male    25.6  31.6 Sobrepesso      
#6 Male    16.9  18.1 Desnutrición    
#7 Male    36.6  29.9 obesidad grado 2
#8 Male    38.6  29.9 obesidad grado 2
#9 Male    24.2  17   Peso normal     
#10 Female  41.4  26   obesidad grado 2
# ℹ 20,748 more rows

# Agrupar por género de tipo de obesidad y resumir por índice de
# masa coroporal
obesity |>
  group_by(Gender, tipo_obesidad) |>
  summarise(
    n_gender_tipo = n()
  ) # produce:
# A tibble: 10 × 3
# Groups:   Gender [2]
#  Gender tipo_obesidad    n_gender_tipo
#  <fct>  <chr>            <int>
#1 Male   Desnutrición       876
#2 Male   Obesidad grado 1  1923
#3 Male   Peso normal       1640
#4 Male   Sobrepesso        2991
#5 Male   obesidad grado 2  2906
#6 Female Desnutrición      1532
#7 Female Obesidad grado 1  1195
#8 Female Peso normal       1886
#9 Female Sobrepesso        1749
#10 Female obesidad grado 2  4060

# Filtrar por tipo de obesidad grado 3, agrupar por género y resumir
# por cantidad
obesity |> 
  filter(tipo_obesidad == "Obesidad grado 3") |>
  group_by(Gender) |>
  summarise(
    n_gender = n()
  )

# Conocer la cantidad de personas por género
obesity |>
  group_by(Gender) |>
  summarise(
    n_gender = n()
  ) # produce:
# A tibble: 2 × 2
#  Gender n_gender
#  <fct>     <int>
#1 Male      10336
#2 Female    10422