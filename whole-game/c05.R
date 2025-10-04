#####################################
###                               ###
###         Capítulo 5            ###
###   Ordenamiento de datos       ### 
###                               ###
#####################################
library(tidyverse)

# Representaciones distintas de los mismos datos con table1, table2 y
# table3
table1 # produce:
#> # A tibble: 6 × 4
#>   country      year  cases population
#>   <chr>       <dbl>  <dbl>      <dbl>
#> 1 Afghanistan  1999    745   19987071
#> 2 Afghanistan  2000   2666   20595360
#> 3 Brazil       1999  37737  172006362
#> 4 Brazil       2000  80488  174504898
#> 5 China        1999 212258 1272915272
#> 6 China        2000 213766 1280428583
table2 # produce:
#> # A tibble: 12 × 4
#>   country      year type           count
#>   <chr>       <dbl> <chr>          <dbl>
#> 1 Afghanistan  1999 cases            745
#> 2 Afghanistan  1999 population  19987071
#> 3 Afghanistan  2000 cases           2666
#> 4 Afghanistan  2000 population  20595360
#> 5 Brazil       1999 cases          37737
#> 6 Brazil       1999 population 172006362
#> # ℹ 6 more rows
table3 # produce:
#> # A tibble: 6 × 3
#>   country      year rate             
#>   <chr>       <dbl> <chr>            
#> 1 Afghanistan  1999 745/19987071     
#> 2 Afghanistan  2000 2666/20595360    
#> 3 Brazil       1999 37737/172006362  
#> 4 Brazil       2000 80488/174504898  
#> 5 China        1999 212258/1272915272
#> 6 China        2000 213766/1280428583

# Hay tres reglas interrelacionadas que hacen que un conjunto de datos 
# esté ordenado:
#
# 1. Cada variable es una columna; cada columna es una variable.
# 2. Cada observación es una fila; cada fila es una observación.
# 3. Cada valor es una celda; cada celda es un valor único.

(4 / 2 * 10) # produce: 20
# Relación de casos por 10000 habitantes
table1 |>
  mutate(rate = (cases / population) * 10000) # produce
#> # A tibble: 6 × 5
#>   country      year  cases population  rate
#>   <chr>       <dbl>  <dbl>      <dbl> <dbl>
#> 1 Afghanistan  1999    745   19987071 0.373
#> 2 Afghanistan  2000   2666   20595360 1.29 
#> 3 Brazil       1999  37737  172006362 2.19 
#> 4 Brazil       2000  80488  174504898 4.61
#> 5 China        1999 212258 1272915272 1.67 
#> 6 China        2000 213766 1280428583 1.67
# Calcular el total de casos por año
table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases)) # produce:
#> # A tibble: 2 × 2
#>    year total_cases
#>   <dbl>       <dbl>
#> 1  1999      250740
#> 2  2000      296920
# Visualizar cambios en el tiempo
# Visualize changes over time
ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000)) # x-axis breaks at 1999 
                                             # and 2000

########################
###
### 5.2.1 Ejercicios
###
########################

# 1. Para cada una de las tablas de muestra, describa qué representa 
# cada observación y cada columna.
#table1:
#> # A tibble: 6 × 4
#>   country      year  cases population
#>   <chr>       <dbl>  <dbl>      <dbl>
#> 1 Afghanistan  1999    745   19987071
#> 2 Afghanistan  2000   2666   20595360
#> 3 Brazil       1999  37737  172006362
#> 4 Brazil       2000  80488  174504898
# En este caso hay cuatro variables país, año, número de casos y 
# población. Las observaciones están representadas por filas que 
# contienen la información de las columnas
# table2
#> # A tibble: 12 × 4
#>   country      year type           count
#>   <chr>       <dbl> <chr>          <dbl>
#> 1 Afghanistan  1999 cases            745
#> 2 Afghanistan  1999 population  19987071
#> 3 Afghanistan  2000 cases           2666
#> 4 Afghanistan  2000 population  20595360
# En este caso hay cuatro variables país, año, tipo y count. Donde 
# tipo puede el número de casos de tuberculosis y population es la
# población total
# Y las observaciones están representadas por filas
# tabla3
#>   country      year rate             
#>   <chr>       <dbl> <chr>            
#> 1 Afghanistan  1999 745/19987071     
#> 2 Afghanistan  2000 2666/20595360    
#> 3 Brazil       1999 37737/172006362  
#> 4 Brazil       2000 80488/174504898 
# Este caso tiene 3 variables país, año y rate que es una cadena 
# de caracteres que representa una relación entre los casos de 
# tuberculosis y la población total

# 2. Describe el proceso que usarías para calcular rate para table2 y 
# table3. Necesitarás realizar cuatro operaciones:
# a. Extraiga el número de casos de tuberculosis por país por año.
# b. Extraiga la población coincidente por país por año.
# c. Divida los casos por la población y multiplíquelo por 10000.
# d. Guárdelo nuevamente en el lugar apropiado.
#table2: 
#>   country      year type           count
#>   <chr>       <dbl> <chr>          <dbl>
#> 1 Afghanistan  1999 cases            745
#> 2 Afghanistan  1999 population  19987071
#> 3 Afghanistan  2000 cases           2666
#> 4 Afghanistan  2000 population  20595360
# Respuesta:
# Este caso es difícil porque para conocer el número de casos de 
# tuberculosis necesito extraer el valor de count siempre que type
# sea cases y para conocer la cantidad de población necesito extraer
# el valor de count siempre que type sea population.
# Pero el problema mayor sería crear la relación en mutate en una
# nueva variable porque debería crear una relación entre dos valores
# de la misma columna, tal vez siempre que country y year tengan
# el mismo valor (Esa relación de dos valores de la misma columna
# es raro)
#table2:
#>   country      year rate             
#>   <chr>       <dbl> <chr>            
#> 1 Afghanistan  1999 745/19987071     
#> 2 Afghanistan  2000 2666/20595360    
#> 3 Brazil       1999 37737/172006362  
#> 4 Brazil       2000 80488/174504898 
# Este caso parece más fácil. Habría que dividir la cadena de caracteres
# de la variable rate, tal vez tomando en cuenta la barra inclinada.
# Con las cadenas separadas o al mismo tiempo de separar las cadenas
# las mutaría en nuevas varibles, una para casos de tuberculosis y
# otra para población total. De ahí mutaría en una nueva variable
# rate que relacionaría los casos de tuberculosis con la población total

billboard 
# billboard es un conjunto de datos que registra la clasificación de 
# las canciones en Billboard en el año 2000. Cada observación (fila)
# es una canción. Desde la cuarta columna las variables wk describen
# la clasificación de la columna cada semana

# produce:
# A tibble: 317 × 79
#  artist    track date.entered   wk1   wk2   wk3   wk4   wk5   wk6   wk7
#  <chr>     <chr> <date>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#1 2 Pac     Baby… 2000-02-26      87    82    72    77    87    94    99
#2 2Ge+her   The … 2000-09-02      91    87    92    NA    NA    NA    NA
#3 3 Doors … Kryp… 2000-04-08      81    70    68    67    66    57    54
#4 3 Doors … Loser 2000-10-21      76    76    72    69    67    65    55

# Ordenar los datos 
billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  ) # produce:
# A tibble: 24,092 × 5
# artist track                   date.entered week   rank
#  <chr>  <chr>                   <date>       <chr> <dbl>
#1 2 Pac  Baby Don't Cry (Keep... 2000-02-26   wk1      87
#2 2 Pac  Baby Don't Cry (Keep... 2000-02-26   wk2      82
#3 2 Pac  Baby Don't Cry (Keep... 2000-02-26   wk3      72
#4 2 Pac  Baby Don't Cry (Keep... 2000-02-26   wk4      77
# Notas importante:
# cols : indica que columnas deben pivotarse es decir cuáles dejarán
# de ser columnas
# names_to : asigna a las variables seleccionadas para que sean los
# nuevos valores de la columna que se llamrá "week"
# values_to : asigna los valores de las variables antiguas a una nueva
# variable llamada "rank"

# values_drop_na : elimina los valores NA de la variable "rank"
billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) # produce:
# A tibble: 5,307 × 5
#  artist  track                   date.entered week   rank
#  <chr>   <chr>                   <date>       <chr> <dbl>
#1 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk1      87
#2 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk2      82
#3 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk3      72
#4 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk4      77
#5 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk5      87
#6 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk6      94
#7 2 Pac   Baby Don't Cry (Keep... 2000-02-26   wk7      99

# parse_number() : extrae el primer número de una cadena de texto
billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(
    week = parse_number(week)
  )
billboard_longer # produce:
# A tibble: 5,307 × 5
#  artist  track                   date.entered  week  rank
#   <chr>   <chr>                   <date>       <dbl> <dbl>
#1 2 Pac   Baby Don't Cry (Keep... 2000-02-26       1    87
#2 2 Pac   Baby Don't Cry (Keep... 2000-02-26       2    82
#3 2 Pac   Baby Don't Cry (Keep... 2000-02-26       3    72
#4 2 Pac   Baby Don't Cry (Keep... 2000-02-26       4    77

# Gráfica que muestra el comportamiento de las canciones a lo largo
# de las semanas. Se observa que muy pocas logran estar en el top 100
# por más de 20 semanas.
billboard_longer |>
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

# tribble() : es una función para construir pequeños tibbles o tablas
df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)
df # produce:
# A tibble: 3 × 3
#  id      bp1   bp2
#  <chr> <dbl> <dbl>
#1 A       100   120
#2 B       140   115
#3 C       120   125
# Se pivotea (mover sobre un eje o sobre sí mismo) df para tener
# una nueva tabla con las variables: id, measurement y value:
df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  ) # produce:
# A tibble: 6 × 3
#  id    measurement value
#  <chr> <chr>       <dbl>
#1 A     bp1           100
#2 A     bp2           120
#3 B     bp1           140
#4 B     bp2           115

who2 # Es una tabla de la OMS que registra información sobre diagnósticos
# de tuberculosis
# produce:
# A tibble: 7,240 × 58
#  country      year sp_m_014 sp_m_1524 sp_m_2534 sp_m_3544 sp_m_4554
#  <chr>       <dbl>    <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#1 Afghanistan  1980       NA        NA        NA        NA        NA
#2 Afghanistan  1981       NA        NA        NA        NA        NA
#3 Afghanistan  1982       NA        NA        NA        NA        NA
#4 Afghanistan  1983       NA        NA        NA        NA        NA

# Pivotear who2 para que la tabla tenga 6 variables. Ya hay country
# y year. En el caso de las otras variables, los nombres se refieren
# a lo siguiente: srp, rel y ep corresponden al método de diagnóstico;
# m y f corresponden al género; y los numeros 1525, 2534, 3544, etc
# corresponden al rango de edad. Con eso se podrían tener hasta 6
# columnas en total.
who2 |>
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  ) # produce:
#> # A tibble: 405,440 × 6
#>   country      year diagnosis gender age   count
#>   <chr>       <dbl> <chr>     <chr>  <chr> <dbl>
#> 1 Afghanistan  1980 sp        m      014      NA
#> 2 Afghanistan  1980 sp        m      1524     NA
#> 3 Afghanistan  1980 sp        m      2534     NA
#> 4 Afghanistan  1980 sp        m      3544     NA
#> 5 Afghanistan  1980 sp        m      4554     NA
#> 6 Afghanistan  1980 sp        m      5564     NA
#> # ℹ 405,434 more rows

household # produce:
#> # A tibble: 5 × 5
#>   family dob_child1 dob_child2 name_child1 name_child2
#>    <int> <date>     <date>     <chr>       <chr>      
#> 1      1 1998-11-26 2000-01-29 Susan       Jose       
#> 2      2 1996-06-22 NA         Mark        <NA>       
#> 3      3 2002-07-11 2004-04-05 Sam         Seth       
#> 4      4 2004-10-10 2009-08-27 Craig       Khai       
#> 5      5 2000-12-05 2005-02-28 Parker      Gracie

# Se pivotea para crear una tabla de 4 variables: family, child, dob
# y name, Tod esto gracias a ".value" que es el primer valor del 
# vector que se le asigna a la variable names_to
household |>
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )
# A tibble: 9 × 4
#  family child  dob        name  
#   <int> <chr>  <date>     <chr> 
#1      1 child1 1998-11-26 Susan 
#2      1 child2 2000-01-29 Jose  
#3      2 child1 1996-06-22 Mark  
#4      3 child1 2002-07-11 Sam   
#5      3 child2 2004-04-05 Seth  
#6      4 child1 2004-10-10 Craig 
#7      4 child2 2009-08-27 Khai  
#8      5 child1 2000-12-05 Parker
#9      5 child2 2005-02-28 Gracie

# cms_patient_experience: Es un conjunto de datos de los centros de 
# servicios de Medicare y Medicaid que recopila datos sobre las 
# experiencias de los pacientes.
# La unidad central estudiada es una organización, pero cada 
# organización se distribuye en seis filas, con una fila por cada 
# medición realizada en la organización de la encuesta.
cms_patient_experience # produce:
#> # A tibble: 500 × 5
#>   org_pac_id org_nm                     measure_cd   measure_title   prf_rate
#>   <chr>      <chr>                      <chr>        <chr>              <dbl>
#> 1 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_1  CAHPS for MIPS…       63
#> 2 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_2  CAHPS for MIPS…       87
#> 3 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_3  CAHPS for MIPS…       86
#> 4 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_5  CAHPS for MIPS…       57
#> 5 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_8  CAHPS for MIPS…       85
#> 6 0446157747 USC CARE MEDICAL GROUP INC CAHPS_GRP_12 CAHPS for MIPS…       24
#> # ℹ 494 more rows

# Podemos ver el conjunto completo de valores para measure_cd y 
# measure_title mediante distinct():
cms_patient_experience |>
  distinct(measure_cd, measure_title) # produce:
#> # A tibble: 6 × 2
#>   measure_cd   measure_title                                                 
#>   <chr>        <chr>                                                         
#> 1 CAHPS_GRP_1  CAHPS for MIPS SSM: Getting Timely Care, Appointments, and In…
#> 2 CAHPS_GRP_2  CAHPS for MIPS SSM: How Well Providers Communicate            
#> 3 CAHPS_GRP_3  CAHPS for MIPS SSM: Patient's Rating of Provider              
#> 4 CAHPS_GRP_5  CAHPS for MIPS SSM: Health Promotion and Education            
#> 5 CAHPS_GRP_8  CAHPS for MIPS SSM: Courteous and Helpful Office Staff        
#> 6 CAHPS_GRP_12 CAHPS for MIPS SSM: Stewardship of Patient Resources

cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  ) # produce:
#> # A tibble: 500 × 9
#>   org_pac_id org_nm                   measure_title   CAHPS_GRP_1 CAHPS_GRP_2
#>   <chr>      <chr>                    <chr>                 <dbl>       <dbl>
#> 1 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          63          NA
#> 2 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          NA          87
#> 3 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          NA          NA
#> 4 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          NA          NA
#> 5 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          NA          NA
#> 6 0446157747 USC CARE MEDICAL GROUP … CAHPS for MIPS…          NA          NA
#> # ℹ 494 more rows
#> # ℹ 4 more variables: CAHPS_GRP_3 <dbl>, CAHPS_GRP_5 <dbl>, …

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  ) # produce:
#> # A tibble: 95 × 8
#>   org_pac_id org_nm           CAHPS_GRP_1 CAHPS_GRP_2 CAHPS_GRP_3 CAHPS_GRP_5
#>   <chr>      <chr>                  <dbl>       <dbl>       <dbl>       <dbl>
#> 1 0446157747 USC CARE MEDICA…          63          87          86          57
#> 2 0446162697 ASSOCIATION OF …          59          85          83          63
#> 3 0547164295 BEAVER MEDICAL …          49          NA          75          44
#> 4 0749333730 CAPE PHYSICIANS…          67          84          85          65
#> 5 0840104360 ALLIANCE PHYSIC…          66          87          87          64
#> 6 0840109864 REX HOSPITAL INC          73          87          84          67
#> # ℹ 89 more rows
#> # ℹ 2 more variables: CAHPS_GRP_8 <dbl>, CAHPS_GRP_12 <dbl>

# Tabla con dos pacientes A y B. A tiene 3 mediciones de presión arterial
# y B tiene 3 mediciones de presión arterial
df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)
# Tomaremos los valores de la columna value y los nombres de la columna 
# measurment.
df |>
  pivot_wider(
    names_from = measurement, # Los valores de measurment serán los nuevos
    # nombres  de las columnas
    values_from = value # los valores de value serán los nuevos valores
    # de las columnas
  ) # produce:

# Para comenzar el proceso, pivot_wider()primero es necesario 
# determinar qué se incluirá en las filas y columnas. Los nuevos 
# nombres de columna serán los valores únicos de measurement.
df |> 
  distinct(measurement) |> 
  pull() # produce:
#[1] "bp1" "bp2" "bp3"

# De forma predeterminada, las filas de la salida se determinan por 
# todas las variables que no se incluyen en los nuevos nombres o 
# valores. Estas se denominan id_cols. Aquí solo hay una columna, 
# pero, en general, puede haber cualquier número.
df |> 
  select(-measurement, -value) |>
  distinct()
#> # A tibble: 2 × 1
#>   id   
#>   <chr>
#> 1 A    
#> 2 B

# pivot_wider()Luego combina estos resultados para generar un marco 
# de datos vacío:
df |> 
  select(-measurement, -value) |>
  distinct() |>
  mutate(x = NA, y = NA, z = NA) # produce:
#> # A tibble: 2 × 4
#>   id    x     y     z    
#>   <chr> <lgl> <lgl> <lgl>
#> 1 A     NA    NA    NA   
#> 2 B     NA    NA    NA
#> # Luego, completa todos los valores faltantes con los datos de 
#> entrada.

