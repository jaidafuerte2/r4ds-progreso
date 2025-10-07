#####################################
###                               ###
###         Capítulo 7            ###
###   Importación de datos        ### 
###                               ###
#####################################

library(tidyverse)

# Primera forma de cargar una tabla, teniendo el archivo .csv en
# algún lugar del directorio actual de trabajo
getwd() # produce: [1] "/cloud/project"
students <- read_csv("data/students.csv")
students # produce: 
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    N/A                Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA   
#5            5 Chidiegwu Dunkel Pizza              Breakfast and l… five 
#6            6 Güvenç Attila    Ice cream          Lunch only       6 

# Segunda forma de cargar una tabla, cargándola desde su dirección en
# internet
students2 <- read_csv("https://pos.it/r4ds-students-csv")
students2 # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    N/A                Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA   
#5            5 Chidiegwu Dunkel Pizza              Breakfast and l… five 
#6            6 Güvenç Attila    Ice cream          Lunch only       6 

# Cambia los valores de cadena "N/A" y "" como valores reales que se
# reconocen como valores no disponibles
students <- read_csv("data/students.csv", na = c("N/A", ""))
students # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    <NA>               Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       <NA>   
#5            5 Chidiegwu Dunkel Pizza              Breakfast and l… five 
#6            6 Güvenç Attila    Ice cream          Lunch only       6 

# Se cambia los nombres de variables 'Student ID' y "Full Name" por
# sus versiones sin espacios vacíos porque en R no es válido un nombre
# de variable con espacios vacíos:
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )
students # produce:
# A tibble: 6 × 5
#`Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    N/A                Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA   
#5            5 Chidiegwu Dunkel Pizza              Breakfast and l… five 
#6            6 Güvenç Attila    Ice cream          Lunch only       6 

# Cambia el tipo de la variable "mealPlan", que es una categoría, con
# un conjunto bien conocido de distintos valores
students |>
  janitor::clean_names() |> # Esta es una especie de preparación de
                            # la tabla
  mutate(meal_plan = factor(meal_plan)) # Cambia el tipo de la variable
                                        # meal_plan de <chr> a <fct>
# produce:
# A tibble: 6 × 5
#student_id full_name        favourite_food     meal_plan          age  
#       <dbl> <chr>            <chr>              <fct>              <chr>
#1          1 Sunil Huffmann   Strawberry yoghurt Lunch only         4    
#2          2 Barclay Lynn     French fries       Lunch only         5    
#3          3 Jayendra Lyne    N/A                Breakfast and lun… 7    
#4          4 Leon Rossini     Anchovies          Lunch only         NA   
#5          5 Chidiegwu Dunkel Pizza              Breakfast and lun… five 
#6          6 Güvenç Attila    Ice cream          Lunch only         6  

students |> janitor::clean_names() # Limpia los nombres de las variables
# y también limpia los NA 
# produce:
#A tibble: 6 × 5
#  student_id full_name        favourite_food     meal_plan          age  
#       <dbl> <chr>            <chr>              <chr>              <chr>
#1          1 Sunil Huffmann   Strawberry yoghurt Lunch only         4    
#2          2 Barclay Lynn     French fries       Lunch only         5    
#3          3 Jayendra Lyne    <NA>               Breakfast and lun… 7    
#4          4 Leon Rossini     Anchovies          Lunch only         <NA>   
#5          5 Chidiegwu Dunkel Pizza              Breakfast and lun… five 
#6          6 Güvenç Attila    Ice cream          Lunch only         6 

# Cambia el tipo de la variable age de tipo caracter (chr) a numérico 
# (dbl)
students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan),
         age = parse_number(if_else(age == "five", "5", age))
  )
students  # produce:
# A tibble: 6 × 5
#student_id full_name        favourite_food     meal_plan            age
#       <dbl> <chr>            <chr>              <fct>              <dbl>
#1          1 Sunil Huffmann   Strawberry yoghurt Lunch only             4
#2          2 Barclay Lynn     French fries       Lunch only             5
#3          3 Jayendra Lyne    <NA>               Breakfast and lun…     7
#4          4 Leon Rossini     Anchovies          Lunch only            <NA<
#5          5 Chidiegwu Dunkel Pizza              Breakfast and lun…     5
#6          6 Güvenç Attila    Ice cream          Lunch only             6
# La función if_else() tiene tres argumentos. El primer argumento es
# un vetor lógico (booleano). El segundo argumento será el resultado
# del vector lógico si resulta TRUE, de otra forma se tomará el tercer
# argumento como resultado del vector lógico.

# Leer una cadena de caracteres que tenga una forma parecida
# a la de un archivo csv
read_csv(
  "a,b,c
  1,2,3
  4,5,6"
) # produce:
# A tibble: 2 × 3
#      a     b     c 
#  <dbl> <dbl> <dbl>
#1     1     2     3
#2     4     5     6

# skip : omite las n primeras líneas de una cadena de caracteres
# que se pase como argumento a la funció read_csv()
read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2
)
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3

# comment : elimina todas las líneas de texto que al inicio tengan
# por ejemplo una "#"
read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3

#col_names = FALSE : indica a read_csv que no trate la primera fila
# como encabezado y que los nombres de las variables se llamen:
# x1, x2, x3, xn, etc.
read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
) # produce:
#> # A tibble: 2 × 3
#>      X1    X2    X3
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
#> 2     4     5     6

# col_names también puede recibir un vector como argumento. Este 
# vector contiene los nombres de las variables.
read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
) # produce:
#> # A tibble: 2 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
#> 2     4     5     6

# Hay otras funciones readr que también son importantes:
# read_csv2() : Lee archivos separados por punto y coma. Estos se 
# utilizan ;en lugar de ,para separar campos y son comunes en países 
# que los usan ,como separador decimal.
# read_tsv() : Lee archivos delimitados por tabulaciones.
# read_delim() : Lee archivos con cualquier delimitador e intenta 
# adivinar automáticamente el delimitador si no lo especifica.
# read_fwf()Lee archivos de ancho fijo. Puedes especificar los 
# campos por su ancho con fwf_widths()o por su posición con 
# fwf_positions().
# read_table()Lee una variación común de archivos de ancho fijo 
# donde las columnas están separadas por espacios en blanco.
# read_log()Lee archivos de registro de estilo Apache.

########################
###
### 7.2.4 Ejercicios
###
########################

# 1. ¿Qué función usarías para leer un archivo donde los campos 
# estuvieran separados por “|”?
# Creo que read_delim()

# 2. Además de file, skip, y comment, ¿qué otros argumentos tienen 
# read_csv()y read_tsv()tienen en común?
?read_csv # : file, col_names, col_types, col_select, id, locale
# na, quoted_na, quote, comment, trims_ws, skip, n_max, guess_max,
# name_repair, num_threads, progress, show_col_types, skip_empty_rows
# lazy
?read_tsv # revisando la documentación se observa que todos los
# argumentos de csv también están disponibles para tsv, son los
# mismos

# 3. ¿Cuales son los argumentos más importantes read_fwf()?
?read_fwf # Es difícil saber con exactitud la respuesta de una 
# pregunta filosófica pero supongo que son: col_positions, col_types
# y col_select

# 4. A veces, las cadenas en un archivo CSV contienen comas. 
# Para evitar problemas, deben ir entre comillas, como "o '. De 
# forma predeterminada, read_csv()se asume que las comillas serán ". 
# Para leer el siguiente texto en un marco de datos, ¿qué argumento 
# read_csv()se debe especificar? "x,y\n1,'a,b'"
?read_csv # creo que quote

# 5. Identifica el problema con cada uno de los siguientes archivos 
# CSV en línea. ¿Qué sucede al ejecutar el código?
read_csv("a,b\n1,2,3\n4,5,6") # produce:
# A tibble: 2 × 2
#      a     b
#  <dbl> <dbl>
#1     1    23
#2     4    56
# En este caso parece que lo que se quiere es una tabla de 3 columnas
# pero se terminan teniendo sólo dos y se juntan 2 con 3 y 5 con 6

read_csv("a,b,c\n1,2\n1,2,3,4") # produce:
# A tibble: 2 × 3
#      a     b     c
#  <dbl> <dbl> <dbl>
#1     1     2    NA
#2     1     2    34
# En este caso se tienen 3 columnas pero la tercera columna tiene
# un NA y 3 y 4 juntos lo que no creo que era la intención

read_csv("a,b\n\"1") # produce:
# A tibble: 0 × 2
# ℹ 2 variables: a <chr>, b <chr>
# En este caso el problema es que hay 0 filas lo que es raro o 
# equivocado

read_csv("a,b\n1,2\na,b") # produce:
# A tibble: 2 × 2
#  a     b    
#  <chr> <chr>
#1 1     2    
#2 a     b 
# Pienso que en este caso el problema es que en las columnas hay tipos
# diferentes aunque se fuerce a los números a ser de tipo chr o
# character

read_csv("a;b\n1;3") # produce:
# A tibble: 1 × 1
#  `a;b`
#  <chr>
#1 1;3  
# En este caso "a;b" y "1;3" actúan como una sola cadena cada uno
# formando una tabla de 1 * 1

# 6. Practique la referencia a nombres no sintácticos en el 
# siguiente marco de datos mediante:
# a. Extrayendo la variable llamada 1.
# b. Trazando un diagrama de dispersión de 1 vs. 2
# c. Creando una nueva columna llamada 3, que está 2dividida por 1.
# d. Renombrar las columnas a one, two, y three.
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying # produce:
# A tibble: 10 × 2
#     `1`   `2`
#   <int> <dbl>
#1     1  2.10
#2     2  3.03
#3     3  5.83
#4     4  6.40
#5     5  8.78
#6     6 12.9 
#7     7 13.7 
#8     8 16.0 
#9     9 17.2 
#10    10 19.1

# a. Extrayendo la variable llamada 1.
annoying |>
  select(`1`) # produce:
# A tibble: 10 × 1
#      `1`
#  <int>
#1     1
#2     2
#3     3
#4     4
#5     5
#6     6
#7     7
#8     8
#9     9
#10    10

# b. Trazando un diagrama de dispersión de 1 vs. 2
annoying |>
  ggplot(aes(x = `1`, y = `2`)) +
  geom_point()

# c. Creando una nueva columna llamada 3, que está 2 dividida por 1.
my_annoying <- annoying |>
  mutate(
    `3` = `2` / `1` 
  ) # produce:
# A tibble: 10 × 3
#     `1`   `2`   `3`
#   <int> <dbl> <dbl>
#1     1  2.10  2.10
#2     2  3.03  1.51
#3     3  5.83  1.94
#4     4  6.40  1.60

# d. Renombrar las columnas a one, two, y three.
my_annoying |>
  select(one = `1`,
         two = `2`,
         three = `3`) # produce:
# A tibble: 10 × 3
#    one   two three
#   <int> <dbl> <dbl>
#1     1  2.10  2.10
#2     2  3.03  1.51
#3     3  5.83  1.94
#4     4  6.40  1.60

# Ejemplo del control de tipos por adivinación de R:
read_csv("
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
") # produce:
#> # A tibble: 3 × 4
#>   logical numeric date       string
#>   <lgl>     <dbl> <date>     <chr> 
#> 1 TRUE        1   2021-01-15 abc   
#> 2 FALSE       4.5 2021-02-15 def   
#> 3 TRUE      Inf   2021-02-16 ghi

# Esta tabla tiene un error en la detección de tipo porque hay valores
# inesperados con un valor distinto al NA
simple_csv <- "
  x
  10
  .
  20
  30"
read_csv(simple_csv) # produce:
#> # A tibble: 4 × 1
#>   x    
#>   <chr>
#> 1 10   
#> 2 .    
#> 3 20   
#> 4 30

# col_types : es un argumento que toma una lista con los nombres
# de las columnas. Y a estos nombres de las columnas les puede forzar
# a tomar un tipo específico.

df <- read_csv(
  simple_csv,
  col_types = list(x = col_double())
) # produce:
#> Warning: One or more parsing issues, call `problems()` on your data frame for
#> details, e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
# Este mensaje nos dice que hubo un problema y podemos encontrar más
# información del problema con la función problems()
problems(df)
# A tibble: 1 × 5
#      row   col expected actual file                          
#    <int> <int> <chr>    <chr>  <chr>                         
#  1     3     1 a double .      /tmp/RtmptYWOBf/file18de3f3243
# Este resultado nos dice que en la fila 3, columna 1 hay actualmente
# un punto (.) en vez del valor esperado que es un "a double" (Dice
# fila 3 porque supongo que la fila con los nombres de las variables
# se cuenta como fila 1). Esto sugiere que este conjunto de datos usa
# un punto (.) para representar valores faltantes

# Al establecer que na = "." , la suposición automática funciona 
# correctamente , obteniendo la columna numérica deseada:
read_csv(simple_csv, na = ".") # produce:
# A tibble: 4 × 1
#x
#  <dbl>
#1    10
#2    NA
#3    20
#4    30

# Hay 9 tipos de columnas: 
# col_logical(), col_double() : se necesitan con poca frecuencia ya
# que r puede identificarlos automaticamente.
# col_integer(): usan menos memoria que los dobles
# col_character() : se le asigna a números que no van a realizar cálculos
# numéricos como la cédula o números de tarjetas de crédito.
# col_factor(), col_date()y col_datetime() :  crean factores, fechas y
# horas respectivamente.
# col_number() : es un analizador numérico permisivo, especialmente 
# útil para monedas.
# cols_skip(): omite una columna para que no se incluya en el resultado

# Es posible anular la columna predetermianda cambiando de list() a 
# cols() y especificando .default:
another_csv <- "
x,y,z
1,2,3"
read_csv(another_csv) # produce:
# A tibble: 1 × 3
#        x     y     z
#    <dbl> <dbl> <dbl>
#  1     1     2     3
read_csv(
  another_csv,
  col_types = cols(.default = col_character())
) # produce:
#> # A tibble: 1 × 3
#>   x     y     z    
#>   <chr> <chr> <chr>
#> 1 1     2     3

# cols_only() : lee sólo las columnas que se especifican
read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
) # produce:
# A tibble: 1 × 1                                                        
#    x    
#    <chr>
#  1 1  
read_csv(
  another_csv,
  col_types = cols_only(col_character())
) # produce:
# A tibble: 1 × 1                                                        
#    x    
#    <chr>
#  1 1  
read_csv(
  another_csv,
  col_types = cols_only()
) # produce: error

# Leer diferentes archivos de datos con tablas de estructura similar:
sales_files <- c("data/01-sales.csv", "data/02-sales.csv", "data/03-sales.csv")
read_csv(sales_files, id = "file") # produce:
#> # A tibble: 19 × 6
#>   file              month    year brand  item     n
#>   <chr>             <chr>   <dbl> <dbl> <dbl> <dbl>
#> 1 data/01-sales.csv January  2019     1  1234     3
#> 2 data/01-sales.csv January  2019     1  8721     9
#> 3 data/01-sales.csv January  2019     1  1822     2
#> 4 data/01-sales.csv January  2019     2  3333     1
# el argumento id añade una nueva columna file al marco de datos 
# resultante que identifica el archivo del que provienen los datos

# las bases de datos también se pueden leer directamente desde sus 
# direcciones:
sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
read_csv(sales_files, id = "file") # produce:
# A tibble: 19 × 6
#  file              month     year brand  item     n
#  <chr>             <chr>    <dbl> <dbl> <dbl> <dbl>
#1 data/01-sales.csv January   2019     1  1234     3
#2 data/01-sales.csv January   2019     1  8721     9
#3 data/01-sales.csv January   2019     1  1822     2
#4 data/01-sales.csv January   2019     2  3333     1

students # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    NA                 Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA   

write_csv(students, "students.csv")
students # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    NA                 Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA 

# Escribe el contenido de una tabla en un nuevo archivo
write_csv(students, "students-2.csv")
read_csv("students-2.csv") # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#         <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    NA                 Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA   
#5            5 Chidiegwu Dunkel Pizza              Breakfast and l… five 
#6            6 Güvenç Attila    Ice cream          Lunch only       6 

# write_rds y read_rds son funciones iguales a write_csv y read_csv
# pero de alguna forma más confiables.
write_rds(students, "students.rds")
read_rds("students.rds") # produce:
#> # A tibble: 6 × 5
#>   student_id full_name        favourite_food     meal_plan             age
#>        <dbl> <chr>            <chr>              <fct>               <dbl>
#> 1          1 Sunil Huffmann   Strawberry yoghurt Lunch only              4
#> 2          2 Barclay Lynn     French fries       Lunch only              5
#> 3          3 Jayendra Lyne    <NA>               Breakfast and lunch     7
#> 4          4 Leon Rossini     Anchovies          Lunch only             NA

library(arrow)
# El paquete arrow permite leer y escribir archivos .parquet, un formato 
# binario rápido que se puede compratir entre lenguajes de programación 
write_parquet(students, "students.parquet")
read_parquet("students.parquet") # produce:
# A tibble: 6 × 5
#  `Student ID` `Full Name`      favourite.food     mealPlan         AGE  
#*        <dbl> <chr>            <chr>              <chr>            <chr>
#1            1 Sunil Huffmann   Strawberry yoghurt Lunch only       4    
#2            2 Barclay Lynn     French fries       Lunch only       5    
#3            3 Jayendra Lyne    NA                 Breakfast and l… 7    
#4            4 Leon Rossini     Anchovies          Lunch only       NA  

# Crear una tabla por columnas
tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
) # produce:
# A tibble: 3 × 3
#      x y         z
#  <dbl> <chr> <dbl>
#1     1 h      0.08
#2     2 m      0.83
#3     5 g      0.6 

# Crear una tabla por filas. Esto puede tener más sentido en el 
# sentido de las filas
tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
) # produce:
# A tibble: 3 × 3
#      x y         z
#  <dbl> <chr> <dbl>
#1     1 h      0.08
#2     2 m      0.83
#3     5 g      0.6 
# Los encabezados de las columnas empiezan con "~" y las entradas se
# separan por comas