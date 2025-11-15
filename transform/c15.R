#####################################
###                               ###
###         Capítulo 15           ###
###         Expresiones           ###
###          Regulares            ###
###                               ###
#####################################

library(tidyverse)
library(babynames)

# Encontrar todas las frutas con terminen o contengan "berry"
str_view(fruit, "berry") # produce:
#>  [6] │ bil<berry>
#>  [7] │ black<berry>
#> [10] │ blue<berry>
#> [11] │ boysen<berry>
#> [19] │ cloud<berry>
#> [21] │ cran<berry>
#> ... and 8 more

# El punto coincide con cualquier caracter. Por ejemplo la siguiente
# expresión coincidirá con cualquier expresión que tenga una "a" y 
# otro caracter
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")
#> [2] │ <ab>
#> [3] │ <ae>
#> [6] │ e<ab>

# También es posible encontrar las frutas que tienen una letra "a"
# seguida de 3 caracteres y que continúa con una "e"
str_view(fruit, "a...e")
#>  [1] │ <apple>
#>  [7] │ bl<ackbe>rry
#> [48] │ mand<arine>
#> [51] │ nect<arine>
#> [62] │ pine<apple>
#> [64] │ pomegr<anate>
#> ... and 2 more

# Cuantificadores: controlan cuantas veces puede coincidir un patrón
# y son: "?", "+", "*"
#
# ? hace que un parámetro sea opcional (coincide 0 o 1 vez)
str_view(c("a", "ab", "abb"), "ab?") # produce:
#> [1] │ <a>
#> [2] │ <ab>
#> [3] │ <ab>b
#
# + permite que un patrón se repita (es decir que se repita al menos
# una vez) : 
str_view(c("a", "ab", "abb"), "ab+") # produce:
#> [2] │ <ab>
#> [3] │ <abb>
#
# * permite que un patrón sea opcional o se repita (es decir, que 
# coincida cualquier número de veces, incluyendo 0)
str_view(c("a", "ab", "abb"), "ab*")
#> [1] │ <a>
#> [2] │ <ab>
#> [3] │ <abb>

# Encontrar las palabras que contienen una "x" rodeada de vocales
# y una "y" rodeada de consonantes:
str_view(words, "[aeiou]x[aeiou]") # produce:
#> [284] │ <exa>ct
#> [285] │ <exa>mple
#> [288] │ <exe>rcise
#> [289] │ <exi>st
#
# Encontrar las palabras que contienen una "y" rodeada de consonantes:
str_view(words, "[^aeiou]y[^aeiou]")
#> [836] │ <sys>tem
#> [901] │ <typ>e

# Buscar frutas que contengan "apple", "melon" o "nut".
str_view(fruit, "apple|melon|nut")
#>  [1] │ <apple>
#> [13] │ canary <melon>
#> [20] │ coco<nut>
#> [52] │ <nut>
#> #
# # Buscar frutas que contengan una vocal repetida
str_view(fruit, "aa|ee|ii|oo|uu")
#>  [9] │ bl<oo>d orange
#> [33] │ g<oo>seberry
#> [47] │ lych<ee>
#> [66] │ purple mangost<ee>n

str_detect(c("a", "b", "c"), "[aeiou]") # produce:
#> [1]  TRUE FALSE FALSE

str_detect(c("a", "b", "c"), "aeiou") # produce:
#> [1]  FALSE FALSE FALSE

# Encontrar todos los nombres más populares que contienen una x 
# minúscula
babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE) # count() agrupa por nombre, 
  # wt = n calcula el peso (el total por nombre) de cada nombre único
  # sort = TRUE ORDENA
# produce:
#> # A tibble: 974 × 2
#>   name           n
#>   <chr>      <int>
#> 1 Alexander 665492
#> 2 Alexis    399551
#> 3 Alex      278705
#> 4 Alexandra 232223
#> # ℹ 968 more rows

babynames |> 
  filter(str_detect(name, "x")) # produce:
# A tibble: 16,317 × 5
#   year sex   name          n      prop
#  <dbl> <chr> <chr>     <int>     <dbl>
#1  1880 F     Roxie        62 0.000635 
#2  1880 F     Dixie        15 0.000154 
#3  1880 F     Roxanna       9 0.0000922
#4  1880 F     Texas         5 0.0000512
#5  1880 M     Alexander   211 0.00178 

# Calcular y visualizar la proporción de nombres de bebés que 
# contienen "x", desglosados por años, parece que aumenta cada año 
babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x")))
# A tibble: 138 × 2
#   year  prop_x
#  <dbl>   <dbl>
#1  1880 0.0065 
#2  1881 0.00879
#3  1882 0.00940
#4  1883 0.00768

x <- c("apple", "banana", "pear")
# Contar cuantas "p" hay en cada cadena
str_count(x, "p")
#> [1] 2 0 1

# Contar cuantas veces coincide una cadena. Tomar en cuenta que las 
# coincidencias de expresiones regulares nunca se superponen 
str_count("abababa", "aba")
#> [1] 2 
str_view("abababa", "aba")
#> [1] │ <aba>b<aba>

# Contar el número de vocales minúsculas y no vocales minúsculas
# que tiene  cada nombre
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )
#> # A tibble: 97,310 × 4
#>   name          n vowels consonants
#>   <chr>     <int>  <int>      <int>
#> 1 Aaban        10      2          3
#> 2 Aabha         5      2          3
#> 3 Aabid         2      2          3
#> 4 Aabir         1      2          3
#> 5 Aabriella     5      4          5
#> 6 Aada          1      2          2
#> # ℹ 97,304 more rows

# Contar cuantas vocales y consonantes tiene un nombre
babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )
#> # A tibble: 97,310 × 4
#>   name          n vowels consonants
#>   <chr>     <int>  <int>      <int>
#> 1 aaban        10      3          2
#> 2 aabha         5      3          2
#> 3 aabid         2      3          2
#> 4 aabir         1      3          2
#> 5 aabriella     5      5          4
#> 6 aada          1      3          1
#> # ℹ 97,304 more rows

x <- c("apple", "pear", "banana")
# Reemplaza todas las coincidencias de vocales minúsculas por un
# guión
str_replace_all(x, "[aeiou]", "-")
#> [1] "-ppl-"  "p--r"   "b-n-n-"

x <- c("apple", "pear", "banana")
# Retira o remueve todas las vocales minúsculas de las cadenas 
# del vector
str_remove_all(x, "[aeiou]")
#> [1] "ppl" "pr"  "bnn"

# Algunos datos derivados de babynames donde tenemos el nombre, el género
# y la edad de un grupo de personas en un formato bastante extraño 
df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)
#
# Extraer los datos de las cadenas de la anterior tabla:
df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", # El primer caracter de la cadena
      name = "[A-Za-z]+", # El nombre con inicial mayúscula seguido
      # de minúsculas
      ">-", # El caracter después de la cadena seguido de un guión
      gender = ".", # Gender es cualquier letra
      "_", # Después del género sigue un guión
      age = "[0-9]+" # Después del guión sigue la edad
    )
  ) # produce:
#> # A tibble: 7 × 3
#>   name    gender age  
#>   <chr>   <chr>  <chr>
#> 1 Sheryl  F      34   
#> 2 Kisha   F      45   
#> 3 Brandon N      33   
#> 4 Sharon  F      38   
#> # ℹ 1 more row

########################
###
### 15.3.5 Ejercicios
###
########################

# No resueltos


# Para crear la expresión regular \. , necesitamos usar \\.
dot <- "\\."
dot # produce: [1] "\\."
#
# Pero la expresión en sí misma solo contiene \
str_view(dot) # produce:
#> [1] │ \.
#
# Esta expresión le dice a R que busque un . explícito
str_view(c("abc", "a.c", "bef"), "a\\.c")
#> [2] │ <a.c>

x <- "a\\b"
x # produce: [1] "a\\b"
str_view(x) # produce:
#> [1] │ a\b
#> Se necesitan cuatro barras invertidas para que coincida una
str_view(x, "\\\\")
#> [1] │ a<\>b

# Una alternativa puede ser usar texto sin formato, lo que permite 
# evitar una capa de escape
str_view(x, r"{\\}") # produce:
#> [1] │ a<\>b

# Usar corchetes es una alternativa a la barra invertida:
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c") # produce:
#> [2] │ <a.c>
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c") # produce:
#> [3] │ <a*c>