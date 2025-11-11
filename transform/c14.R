#####################################
###                               ###
###         Capítulo 14           ###
###           Cadenas             ###
###                               ###
#####################################

library(tidyverse)
library(babynames)

# NOTA: Una función stringr siempre, siempre comienza con str_

# Las cadenas pueden ir entre comillas simples y dobles (igual que en 
# python). Pero por covención es mejor usar comillas dobles
string1 <- "This is a string"
# Esta regla tiene la excepción de que si hay un texto entrecomillado
# dentro de una cadena, es mejor que la cadena use comillas simples.
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

# Para incluir comillas simples o dobles literales en una cadena 
# se debe usar / para escaparlas. De esta forma
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
# Así mismo. si se quiere incluir una barra invertida literal en una
# cadena, hay que escaparla:
backslash <- "\\"
# Veamos los retornos:
x <- c(single_quote, double_quote, backslash)
x # produce: [1] "'"  "\"" "\\" # Esto no parece lo esperado
# Así que para ver el contenido sin formato de la cadena  se puede usar
# la función str_view()
str_view(x) # produce:
#[1] │ '
#[2] │ "
#[3] │ \

# Fijarse que en este ejemplo hay demasiadas barras invertidas (en 
# lo personal ni entiendo bien lo que significa :
tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
str_view(tricky) # produce:
#[1] │ double_quote <- "\"" # or '"'
#│ single_quote <- '\'' # or "'"
#
# Para evitar tantas barras invertidas se puede usar una caedna 
# sin formato:
tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
str_view(tricky) # produce:
#[1] │ double_quote <- "\"" # or '"'
#│ single_quote <- '\'' # or "'"
# Estas cadenas empiezan con "( y terminan con )" pero si una cadena
# incluye "( o )" se puede usar en su lugar r[] o r{}

# otros caracteres especiales son \n (para salto de línea) \t (para
# tabulación), y secuencias de escape unicode \u y \U para caracteres
# no ingleses
x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x # produce:
#[1] "one\ntwo" "one\ttwo" "µ"        "😄"  
str_view(x) # produce: 
#[1] │ one
#│ two
#[2] │ one{\t}two
#[3] │ µ
#[4] │ 😄

########################
###
### 14.2.4 Ejercicios
###
########################

# 1. Crea cadenas que contengan los siguientes valores:
# He said "That's amazing!"
x <- r"(He said "That's amazing!")" # produce:
str_view(x) # produce:
#[1] │ He said "That's amazing!"
# \a\b\c\d
x <- r"(\a\b\c\d)"
str_view(x) # produce:
#[1] │ \a\b\c\d

# 2. Crea la cadena en tu sesión de R e imprímela. ¿Qué ocurre con 
# el carácter especial “\u00a0”? ¿Cómo se str_view()muestra? ¿Puedes 
# buscar en Google para averiguar qué es este carácter especial?
x <- "This\u00a0is\u00a0tricky"
x
str_view(x) # produce:
#[1] "This is tricky"
#> str_view(x)
#[1] │ This{\u00a0}is{\u00a0}tricky
# google dice que es un espacio sin separación o no rompible. Es algo
# como un caracter como un espacio normal pero evita que un salto de 
# línea ocurra entre el y el siguiente caracter. Parece que es útil
# para mantener unidas ciertas palabras como nombre o título para que 
# no se separen en líneas diferentes

str_c("x", "y")
#> [1] "xy"
str_c("x", "y", "z")
#> [1] "xyz"
str_c("Hello ", c("John", "Susan"))
#> [1] "Hello John"  "Hello Susan"
df <- tibble(name = c("Flora", "David", "Terra", NA))
df # produce:
# A tibble: 4 × 1
#  name 
#  <chr>
#1 Flora
#2 David
#3 Terra
#4 NA  
df |> mutate(greeting = str_c("Hi ", name, "!")) # produce:
# A tibble: 4 × 2
#  name   greeting 
#  <chr>     <chr>    
#1 Flora Hi Flora!
#2 David Hi David!
#3 Terra Hi Terra!
#4 NA    NA 
# Se puede usar la función coalesce() para que los valores faltantes
# se muestren de otra manera
df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )
#> # A tibble: 4 × 3
#>   name  greeting1 greeting2
#>   <chr> <chr>     <chr>    
#> 1 Flora Hi Flora! Hi Flora!
#> 2 David Hi David! Hi David!
#> 3 Terra Hi Terra! Hi Terra!
#> 4 <NA>  Hi you!   Hi!

# Con str_glue() todo lo que está entre {} se evaluará como si estuviera
# fuera de comillas
df |> mutate(greeting = str_glue("Hi {name}!"))
#> # A tibble: 4 × 2
#>   name  greeting 
#>   <chr> <glue>   
#> 1 Flora Hi Flora!
#> 2 David Hi David!
#> 3 Terra Hi Terra!
#> 4 <NA>  Hi NA!
#
# Con Glue , si quiero mostrar los caracteres especiales, sólo
# debo duplicarlos
df |> mutate(greeting = str_glue("{{Hi {name}!}}")) # produce:
#> # A tibble: 4 × 2
#>   name  greeting   
#>   <chr> <glue>     
#> 1 Flora {Hi Flora!}
#> 2 David {Hi David!}
#> 3 Terra {Hi Terra!}
#> 4 <NA>  {Hi NA!}

# str_flatten() : podría ser muy bueno para usar con summarize()
str_flatten(c("x", "y", "z")) # produce:
#> [1] "xyz"
str_flatten(c("x", "y", "z"), ", ") # produce:
#> [1] "x, y, z"
str_flatten(c("x", "y", "z"), ", ", last = ", and ") # produce:
#> [1] "x, y, and z"
#
df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", ")) # produce:
#> # A tibble: 3 × 2
#>   name    fruits                      
#>   <chr>   <chr>                       
#> 1 Carmen  banana, apple               
#> 2 Marvin  nectarine                   
#> 3 Terence cantaloupe, papaya, mandarin