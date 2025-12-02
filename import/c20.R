#####################################
###                               ###
###         Capítulo 20           ###
###           Hojas de            ###
###           cálculo             ###
###                               ###
#####################################

library(readxl)
library(tidyverse)
library(writexl)
library(googlesheets4)

# Leer el archivo con su ruta. En posit cloud la ruta se especifica
# desde el directorio de trabajo actual, no desde el directorio del
# archivo:
getwd() # produce: [1] "/cloud/project"
students <- read_excel("r4ds-progreso/import/data/students.xlsx")

students # produce:
#> # A tibble: 6 × 5
#>   `Student ID` `Full Name`      favourite.food     mealPlan            AGE  
#>          <dbl> <chr>            <chr>              <chr>               <chr>
#> 1            1 Sunil Huffmann   Strawberry yoghurt Lunch only          4    
#> 2            2 Barclay Lynn     French fries       Lunch only          5    
#> 3            3 Jayendra Lyne    N/A                Breakfast and lunch 7    
#> 4            4 Leon Rossini     Anchovies          Lunch only          <NA> 
#> 5            5 Chidiegwu Dunkel Pizza              Breakfast and lunch five 
#> 6            6 Güvenç Attila    Ice cream          Lunch only          6

# Esto no funcionó porque la anterior fila de nombres de columna
# pasó a ser la primera fila de la tabla. Para solucionar esto se
# puede usar el argumento skip:
read_excel(
  "r4ds-progreso/import/data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1
)
#> # A tibble: 6 × 5
#>   student_id full_name        favourite_food     meal_plan           age  
#>        <dbl> <chr>            <chr>              <chr>               <chr>
#> 1          1 Sunil Huffmann   Strawberry yoghurt Lunch only          4    
#> 2          2 Barclay Lynn     French fries       Lunch only          5    
#> 3          3 Jayendra Lyne    N/A                Breakfast and lunch 7    
#> 4          4 Leon Rossini     Anchovies          Lunch only          <NA> 
#> 5          5 Chidiegwu Dunkel Pizza              Breakfast and lunch five 
#> 6          6 Güvenç Attila    Ice cream          Lunch only          6

# También se puede usar el argumento na para especificar que una cadena
# vacía y un N/A se usan como valores faltantes:
read_excel(
  "r4ds-progreso/import/data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A")
) # produce:
#> # A tibble: 6 × 5
#>   student_id full_name        favourite_food     meal_plan           age  
#>        <dbl> <chr>            <chr>              <chr>               <chr>
#> 1          1 Sunil Huffmann   Strawberry yoghurt Lunch only          4    
#> 2          2 Barclay Lynn     French fries       Lunch only          5    
#> 3          3 Jayendra Lyne    <NA>               Breakfast and lunch 7    
#> 4          4 Leon Rossini     Anchovies          Lunch only          <NA> 
#> 5          5 Chidiegwu Dunkel Pizza              Breakfast and lunch five 
#> 6          6 Güvenç Attila    Ice cream          Lunch only          6

# Otro problema es que la variable age se interpreta como una 
# columna de caracteres porque a pesar de que tiene numeros, hay 
# una cadena. Para cambiar esto podemos usar col_types para especificar
# los tipos de datos de cada variable explícitamente
read_excel(
  "r4ds-progreso/import/data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "numeric")
) # produce:
#> Warning: Expecting numeric in E6 / R6C5: got 'five'
#> # A tibble: 6 × 5
#>   student_id full_name        favourite_food     meal_plan             age
#>        <dbl> <chr>            <chr>              <chr>               <dbl>
#> 1          1 Sunil Huffmann   Strawberry yoghurt Lunch only              4
#> 2          2 Barclay Lynn     French fries       Lunch only              5
#> 3          3 Jayendra Lyne    <NA>               Breakfast and lunch     7
#> 4          4 Leon Rossini     Anchovies          Lunch only             NA
#> 5          5 Chidiegwu Dunkel Pizza              Breakfast and lunch    NA
#> 6          6 Güvenç Attila    Ice cream          Lunch only              6

# NOTA: esto no funcionó tan bien porque la cadena "five" que estaba 
# en la columna de age, cambió a valor ausente NA. Para solucionar 
# esto primero se debe cambiar la cadena (en modo text) a un valor
# numérico:
students <- read_excel(
  "r4ds-progreso/import/data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)
students # produce:
# A tibble: 6 × 5
#  student_id full_name        favourite_food     meal_plan          age  
#       <dbl> <chr>            <chr>              <chr>              <chr>
#1          1 Sunil Huffmann   Strawberry yoghurt Lunch only         4.0  
#2          2 Barclay Lynn     French fries       Lunch only         5.0  
#3          3 Jayendra Lyne    NA                 Breakfast and lun… 7.0  
#4          4 Leon Rossini     Anchovies          Lunch only         NA   
#5          5 Chidiegwu Dunkel Pizza              Breakfast and lun… five 
students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age) # Extraer dígitos y convertirlos a 
                            # números
  ) 
#
students
#> # A tibble: 6 × 5
#>   student_id full_name        favourite_food     meal_plan             age
#>        <dbl> <chr>            <chr>              <chr>               <dbl>
#> 1          1 Sunil Huffmann   Strawberry yoghurt Lunch only              4
#> 2          2 Barclay Lynn     French fries       Lunch only              5
#> 3          3 Jayendra Lyne    <NA>               Breakfast and lunch     7
#> 4          4 Leon Rossini     Anchovies          Lunch only             NA
#> 5          5 Chidiegwu Dunkel Pizza              Breakfast and lunch     5
#> 6          6 Güvenç Attila    Ice cream          Lunch only              6

# Leer un hoja de cálculo de excel con su ruta y especificar una hoja 
# de trabajo, en este caso: sheet = "Torgersen Island"
read_excel("r4ds-progreso/import/data/penguins.xlsx", 
           sheet = "Torgersen Island")
#> # A tibble: 52 × 8
#>   species island    bill_length_mm     bill_depth_mm      flipper_length_mm
#>   <chr>   <chr>     <chr>              <chr>              <chr>            
#> 1 Adelie  Torgersen 39.1               18.7               181              
#> 2 Adelie  Torgersen 39.5               17.399999999999999 186              
#> 3 Adelie  Torgersen 40.299999999999997 18                 195              
#> 4 Adelie  Torgersen NA                 NA                 NA               
#> 5 Adelie  Torgersen 36.700000000000003 19.3               193              
#> 6 Adelie  Torgersen 39.299999999999997 20.6               190              
#> # ℹ 46 more rows
#> # ℹ 3 more variables: body_mass_g <chr>, sex <chr>, year <dbl>

# NOTA: Algunas variables que contienen datos numéricos se leen como
# caracteres debido a que la cadena de caracteres "NA" no se reconoce
# como verdadera NA. Para solucionar esto se puede especificar la
# variable na
penguins_torgersen <- read_excel("r4ds-progreso/import/data/penguins.xlsx", 
                                 sheet = "Torgersen Island", 
                                 na = "NA")
#
penguins_torgersen # produce:
#> # A tibble: 52 × 8
#>   species island    bill_length_mm bill_depth_mm flipper_length_mm
#>   <chr>   <chr>              <dbl>         <dbl>             <dbl>
#> 1 Adelie  Torgersen           39.1          18.7               181
#> 2 Adelie  Torgersen           39.5          17.4               186
#> 3 Adelie  Torgersen           40.3          18                 195
#> 4 Adelie  Torgersen           NA            NA                  NA
#> 5 Adelie  Torgersen           36.7          19.3               193
#> 6 Adelie  Torgersen           39.3          20.6               190
#> # ℹ 46 more rows
#> # ℹ 3 more variables: body_mass_g <dbl>, sex <chr>, year <dbl>

# Conocer cuáles son las hojas de trabajo de una hoja de cálculo
excel_sheets("r4ds-progreso/import/data/penguins.xlsx")  # produce:
#> [1] "Torgersen Island" "Biscoe Island"    "Dream Island"

# Cuando ya se conocen las hojas de trabajo es más fácil leerlas 
# una por una
penguins_biscoe <- read_excel("r4ds-progreso/import/data/penguins.xlsx", 
                              sheet = "Biscoe Island", na = "NA")
penguins_biscoe # produce:
# A tibble: 168 × 8
# species island bill_length_mm bill_depth_mm flipper_length_mm
#   <chr>   <chr>           <dbl>         <dbl>             <dbl>
#1 Adelie  Biscoe           37.8          18.3               174
#2 Adelie  Biscoe           37.7          18.7               180
#3 Adelie  Biscoe           35.9          19.2               189
#4 Adelie  Biscoe           38.2          18.1               185
# 
penguins_dream  <- read_excel("r4ds-progreso/import/data/penguins.xlsx", 
                              sheet = "Dream Island", na = "NA")
penguins_dream # produce:
# A tibble: 124 × 8
#  species island bill_length_mm bill_depth_mm flipper_length_mm
#  <chr>   <chr>           <dbl>         <dbl>             <dbl>
#1 Adelie  Dream            39.5          16.7               178
#2 Adelie  Dream            37.2          18.1               178
#3 Adelie  Dream            39.5          17.8               188
#4 Adelie  Dream            40.9          18.9               184

# Conocer el número de filas y columnas (en ese orden) con dim
dim(penguins_torgersen) # produce:
#> [1] 52  8
dim(penguins_biscoe) # produce:
#> [1] 168   8
dim(penguins_dream) # produce:
#> [1] 124   8

# Juntar varias hojas de trabajo en una sóla tabla:
penguins <- bind_rows(penguins_torgersen, penguins_biscoe, 
                      penguins_dream)
penguins # produce:
#> # A tibble: 344 × 8
#>   species island    bill_length_mm bill_depth_mm flipper_length_mm
#>   <chr>   <chr>              <dbl>         <dbl>             <dbl>
#> 1 Adelie  Torgersen           39.1          18.7               181
#> 2 Adelie  Torgersen           39.5          17.4               186
#> 3 Adelie  Torgersen           40.3          18                 195
#> 4 Adelie  Torgersen           NA            NA                  NA
#> 5 Adelie  Torgersen           36.7          19.3               193
#> 6 Adelie  Torgersen           39.3          20.6               190
#> # ℹ 338 more rows
#> # ℹ 3 more variables: body_mass_g <dbl>, sex <chr>, year <dbl>

# deaths.xlsx es una hoja de cálculo que es parte del paquete readxl
# y la función readxl_example se puede usar para localizar esta
# hoja de cálculo donde está instalado el paquete en el sistema. Es
# decir que readxl_example devuelve la ruta de la hoja de cálculo
# que luego usa read_excel()
deaths_path <- readxl_example("deaths.xlsx")
deaths <- read_excel(deaths_path)
#> New names:
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#> • `` -> `...5`
#> • `` -> `...6`
deaths
#> # A tibble: 18 × 6
#>   `Lots of people`    ...2       ...3  ...4     ...5          ...6           
#>   <chr>               <chr>      <chr> <chr>    <chr>         <chr>          
#> 1 simply cannot resi… <NA>       <NA>  <NA>     <NA>          some notes     
#> 2 at                  the        top   <NA>     of            their spreadsh…
#> 3 or                  merging    <NA>  <NA>     <NA>          cells          
#> 4 Name                Profession Age   Has kids Date of birth Date of death  
#> 5 David Bowie         musician   69    TRUE     17175         42379          
#> 6 Carrie Fisher       actor      60    TRUE     20749         42731          
#> # ℹ 12 more rows
# NOTA: Las tres primeras y las cuatro últimas filas no forman 
# parte del marco de datos.

# Entonces para eliminar estas filas, se puede usar el parámetro
# range de la función read_excel, especificando la celda donde 
# empieza la tablas y donde termina
read_excel(deaths_path, range = "A5:F15")
#> # A tibble: 10 × 6
#>   Name          Profession   Age `Has kids` `Date of birth`    
#>   <chr>         <chr>      <dbl> <lgl>      <dttm>             
#> 1 David Bowie   musician      69 TRUE       1947-01-08 00:00:00
#> 2 Carrie Fisher actor         60 TRUE       1956-10-21 00:00:00
#> 3 Chuck Berry   musician      90 TRUE       1926-10-18 00:00:00
#> 4 Bill Paxton   actor         61 TRUE       1955-05-17 00:00:00
#> 5 Prince        musician      57 TRUE       1958-06-07 00:00:00
#> 6 Alan Rickman  actor         69 FALSE      1946-02-21 00:00:00
#> # ℹ 4 more rows
#> # ℹ 1 more variable: `Date of death` <dttm>

bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)
#
bake_sale # produce:
#> # A tibble: 3 × 2
#>   item    quantity
#>   <fct>      <dbl>
#> 1 brownie       10
#> 2 cupcake        5
#> 3 cookie         8
#
# Es posible escribir el contenido de una tabla en un archivo de
# excel con la función write_xlsx:
write_xlsx(bake_sale, path = "r4ds-progreso/import/data/bake-sale.xlsx")
#
# Y se puede colver a leer este archivo cuando ya está guardado
read_excel("r4ds-progreso/import/data/bake-sale.xlsx")
#> # A tibble: 3 × 2
#>   item    quantity
#>   <chr>      <dbl>
#> 1 brownie       10
#> 2 cupcake        5
#> 3 cookie         8

# NOTA: posit clous dejó de funcionar cuando quise leer los archivos
# de la hoja de cálculos de google. Es mejor no intentar estas secciones que leen 
# archivos directamente en la nube.

