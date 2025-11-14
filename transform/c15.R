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