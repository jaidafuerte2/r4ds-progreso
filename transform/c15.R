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

# Para que una expresión regular coincida con el inicio de una 
# cadena se debe anclar ^. Y para que coincida con el final se debe
# usar $
str_view(fruit, "^a") # produce:
#> [1] │ <a>pple
#> [2] │ <a>pricot
#> [3] │ <a>vocado
str_view(fruit, "a$") # produce:
#>  [4] │ banan<a>
#> [15] │ cherimoy<a>
#> [30] │ feijo<a>
#> [36] │ guav<a>
#> [56] │ papay<a>
#> [74] │ satsum<a>
#> NOTA: Por defecto las expresiones regulares coinciden con cualquier 
#> parte de una cadena por eso a veces se necesita especificar

# Para forzar una expresión regular a que coincida sólo con la cadena
# completa s debe usa ^ y $
str_view(fruit, "apple") # produce:
#>  [1] │ <apple>
#> [62] │ pine<apple>
str_view(fruit, "^apple$") # produce:
#> [1] │ <apple>

# Otra opción para hacer coincider exacto una expresión regular
# es usar al inicio y al final \\b:
x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum") # produce:
#> [1] │ <sum>mary(x)
#> [2] │ <sum>marize(df)
#> [3] │ row<sum>(x)
#> [4] │ <sum>(x)
str_view(x, "\\bsum\\b") # produce:
#> [4] │ <sum>(x)

# Cuando se usan solas, las anclas producirán una coincidencia 
# de ancho cero:
str_view("abc", c("$", "^", "\\b"))
#> [1] │ abc<>
#> [2] │ <>abc
#> [3] │ <>abc<>

# Esto le ayuda a comprender qué sucede cuando reemplaza una ancla 
# independiente:
str_replace_all("abc", c("$", "^", "\\b"), "--") # produce:
#> [1] "abc--"   "--abc"   "--abc--"

# Clase o conjunto de caracteres que se define entre corchetes.
# Hay dos caracteres importantes "-" y "\".
x <- "abcd ABCD 12345 -!@#%." 
str_view(x, "[abc]+") # produce:
#> [1] │ <abc>d ABCD 12345 -!@#%.
# Muestra coincidencias con todas las letras minúsculas
str_view(x, "[a-z]+") # produce:
#> [1] │ <abcd> ABCD 12345 -!@#%.
# Muetra no coincidencias con letras minúsculas y números 
str_view(x, "[^a-z0-9]+") # produce:
#> [1] │ abcd< ABCD >12345< -!@#%.>
#
# Muestra todas las coinidencias de a hasta c incluida
str_view("a-b-c", "[a-c]") # produce:
#> [1] │ <a>-<b>-<c>
str_view("a-b-c", "[a\\-c]")
# Parece que las barras escapan los guiones. Así que "\\-" hace
# coincidir guiones de una cadena
#> [1] │ <a><->b<-><c>

x <- "abcd ABCD 12345 -!@#%."
# Mostrar coincidencias con cualquier digito
str_view(x, "\\d+") # produce:
#> [1] │ abcd ABCD <12345> -!@#%.
#> Mostrar coincidencias con lo que no sea un digito
str_view(x, "\\D+") # produce:
#> [1] │ <abcd ABCD >12345< -!@#%.>
# Mostrar coincidencias con espacios en blanco, tabulaciones, etc
str_view(x, "\\s+") # produce:
#> [1] │ abcd< >ABCD< >12345< >-!@#%.
#> Mostrar coincidencias con no espacios en blanco
str_view(x, "\\S+") # produce:
#> [1] │ <abcd> <ABCD> <12345> <-!@#%.>
#> Mostrar coincidencias con palabras (letras y números)
str_view(x, "\\w+") # produce:
#> [1] │ <abcd> <ABCD> <12345> -!@#%.
# Mostrar coincidencias con no palabras
str_view(x, "\\W+") # produce:
#> [1] │ abcd< >ABCD< >12345< -!@#%.>

########################
###
### 15.4.7 Ejercicios
###
########################

# NO RESUELTOS

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
#> [1] │ <banana>
#> ingnore_case es un "FLAG" para hacer coincidir las cadenas 
#> independientemente de que estén capitalizados sus caracteres 
str_view(bananas, regex("banana", ignore_case = TRUE))
#> [1] │ <banana>
#> [2] │ <Banana>
#> [3] │ <BANANA>

# dotall también es un "flag" que se puede usar para incluir 
# saltos de línea en la expresión regular
x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
#> ✖ Empty `string` provided.
str_view(x, regex(".Line", dotall = TRUE))
#> [1] │ Line 1<
#>     │ Line> 2<
#>     │ Line> 3

# multiline es otra flag que hace coincidir el inicio y el final 
# de cada línea en vez del inicio y el final de la cadena completa
x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line") # produce:
#> [1] │ <Line> 1
#>     │ Line 2
#>     │ Line 3
str_view(x, regex("^Line", multiline = TRUE)) # produce:
#> [1] │ <Line> 1
#>     │ <Line> 2
#>     │ <Line> 3

# fixed() permite no usar las reglas de las expresiones regulares
str_view(c("", "a", "."), fixed(".")) # produce:
#> [3] │ <.>

# fixed() también permite ignorar mayúsculas y minúsculas
str_view("x X", "X") # produce:
#> [1] │ x <X>
str_view("x X", fixed("X", ignore_case = TRUE))
#> [1] │ <x> <X>

# Buscar todas las oraciones que comiencen con "The", usar solo
# el ancla "^" no es suficiente 
str_view(sentences, "^The") # produce: 
#>  [1] │ <The> birch canoe slid on the smooth planks.
#>  [4] │ <The>se days a chicken leg is a rare dish.
#>  [6] │ <The> juice of lemons makes fine punch.
#>  [7] │ <The> box was thrown beside the parked truck.
#>  [8] │ <The> hogs were fed chopped corn and garbage.
#> [11] │ <The> boy was there when the sun rose.
#> ... and 271 more
# NOTA: este enfoque no es suficiente porque también hace coincidir
# con palabras como they o these

# Para que la coincidencia sea exactamente igual a la cadena que
# se quiere, se puede usar un límite de palabra:
str_view(sentences, "^The\\b")
#>  [1] │ <The> birch canoe slid on the smooth planks.
#>  [6] │ <The> juice of lemons makes fine punch.
#>  [7] │ <The> box was thrown beside the parked truck.
#>  [8] │ <The> hogs were fed chopped corn and garbage.
#> [11] │ <The> boy was there when the sun rose.
#> [13] │ <The> source of the huge river is the clear spring.
#> ... and 250 more

# Buscar todas las palabras que comienzan por un pronombre
str_view(sentences, "^She|He|It|They\\b")
#>  [3] │ <It>'s easy to tell the depth of a well.
#> [15] │ <He>lp the woman get back to her feet.
#> [27] │ <He>r purse was full of useless trash.
#> [29] │ <It> snowed, rained, and hailed the same morning.
#> [63] │ <He> ran half way to the hardware store.
#> [90] │ <He> lay prone and hardly moved a limb.
#> ... and 57 more
# NOTA: algunas coincidencias son érroneas porque no se usó
# paréntesis 

# Hay que añadir el paréntesis
str_view(sentences, "^(She|He|It|They)\\b") # produce:
#>   [3] │ <It>'s easy to tell the depth of a well.
#>  [29] │ <It> snowed, rained, and hailed the same morning.
#>  [63] │ <He> ran half way to the hardware store.
#>  [90] │ <He> lay prone and hardly moved a limb.
#> [116] │ <He> ordered peach pie with ice cream.
#> [127] │ <It> caught its hind paw in a rusty trap.
#> ... and 51 more

# Para darme cuenta de los errores en el enfoque de una expresión
# regular, se podría testear creando unos vectores con cadenas:
pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")
#
pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern) # produce:
#> [1] TRUE TRUE
str_detect(neg, pattern) # produce:
#> [1] FALSE FALSE

# Encontrar palabras que sólo contengan consonantes. Para esto se crea
# una clase de caracter que sólo contenga todaslas letras excepto
# vocales "[^aeiou]". Luego permitir que coinicida con cualquier 
# número de letras "[^aeiou]+" . Y al final forzarla a que coincida
# con toda la cadena anclándola al principio y al final "^[^aeiou]+$":
str_view(words, "^[^aeiou]+$") # produce:
#> [123] │ <by>
#> [249] │ <dry>
#> [328] │ <fly>
#> [538] │ <mrs>
#> [895] │ <try>
#> [952] │ <why>
#
# Se podría simplificar esta locura dándole la vuelta al problema,
# se podría buscar palabras que no contengan ninguna vocal.
str_view(words[!str_detect(words, "[aeiou]")]) # produce:
#> [1] │ by
#> [2] │ dry
#> [3] │ fly
#> [4] │ mrs
#> [5] │ try
#> [6] │ why

# Buscar todas las palabras que contengan una "a" seguida de una "b"
# o tadas las palabras que contengan una "b" seguida de una "a"
str_view(words, "a.*b|b.*a") # produce:
#>  [2] │ <ab>le
#>  [3] │ <ab>out
#>  [4] │ <ab>solute
#> [62] │ <availab>le
#> [66] │ <ba>by
#> [67] │ <ba>ck
#> ... and 24 more

# Combinar los resultados de dos llamadas a str_detect()
words[str_detect(words, "a") & str_detect(words, "b")] # produce:
#>  [1] "able"      "about"     "absolute"  "available" "baby"      "back"     
#>  [7] "bad"       "bag"       "balance"   "ball"      "bank"      "bar"      
#> [13] "base"      "basis"     "bear"      "beat"      "beauty"    "because"  
#> [19] "black"     "board"     "boat"      "break"     "brilliant" "britain"  
#> [25] "debate"    "husband"   "labour"    "maybe"     "probable"  "table"

# Encontrar todas las oraciones que contengan un color. Para esto se 
# combina la alternancia con los límites de las palabras

str_view(sentences, "\\b(red|green|blue)\\b")
#>   [2] │ Glue the sheet to the dark <blue> background.
#>  [26] │ Two <blue> fish swam in the tank.
#>  [92] │ A wisp of cloud hung in the <blue> air.
#> [148] │ The spot on the blotter was made by <green> ink.
#> [160] │ The sofa cushion is <red> and of light weight.
#> [174] │ The sky that morning was clear and bright <blue>.
#> ... and 20 more

# Una opción menos tediosa prodría ser crear un patrón a partir 
# de un vector:
rgb <- c("red", "green", "blue")
str_c("\\b(", str_flatten(rgb, "|"), ")\\b") # produce:
#> [1] "\\b(red|green|blue)\\b"

# Se puede crear un patrón más completo usando la lista de colores 
# predefinidos por r:
str_view(colors()) # produce:
#> [1] │ white
#> [2] │ aliceblue
#> [3] │ antiquewhite
#> [4] │ antiquewhite1
#> [5] │ antiquewhite2
#> [6] │ antiquewhite3
#> ... and 651 more
# 
# Primero hay que eliminar las variables numeradas
cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols) # produce:
#> [1] │ white
#> [2] │ aliceblue
#> [3] │ antiquewhite
#> [4] │ aquamarine
#> [5] │ azure
#> [6] │ beige
#> ... and 137 more
#
# Al final se puede convertir esto en un patrón gigante
pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern) # produce:
#>   [2] │ Glue the sheet to the dark <blue> background.
#>  [12] │ A rod is used to catch <pink> <salmon>.
#>  [26] │ Two <blue> fish swam in the tank.
#>  [66] │ Cars and busses stalled in <snow> drifts.
#>  [92] │ A wisp of cloud hung in the <blue> air.
#> [112] │ Leaves turn <brown> and <yellow> in the fall.
#> ... and 57 more

########################
###
### 15.6.4 Ejercicios
###
########################

# NO RESUELTOS



# Buscar todos los objetos disponibles en el entorno global de r
# que coincidan con el patrón dado.
apropos("replace") # produce:
#> [1] "%+replace%"       "replace"          "replace_na"      
#> [4] "replace_theme"    "setReplaceMethod" "str_replace"     
#> [7] "str_replace_all"  "str_replace_na"   "theme_replace"

