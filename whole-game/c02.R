#####################################
###                               ###
###         Capítulo 2            ###
###     Flujo de trabajo:         ### 
###     Conceptos básicos         ###
###                               ###
#####################################

1 / 200 * 30 # produce:[1] 0.15
(59 + 73 + 2) / 3 # produce: [1] 44.66667
sin(pi / 2) # produce: 1

x <- 3 * 4
x # produce: 12

primes <- c(2, 3, 5, 7, 11, 13)
primes * 2 # produce: [1]  4  6 10 14 22 26
primes - 1 # produce: [1]  1  2  4  6 10 12

# Create vector of primes
primes <- c(2, 3, 5, 7, 11, 13)
# Multiply primes by 2
primes * 2 # produce: [1]  4  6 10 14 22 26

# Los nombres de los objetos deben empezar con una letra y solo 
# pueden contener letras, números, _y ..
# Por ejemplo: 
#i_use_snake_case
#otherPeopleUseCamelCase
#some.people.use.periods
#And_aFew.People_RENOUNCEconvention

x # produce: 12
this_is_a_really_long_name <- 2.5
this_is_a_really_long_name # produce: 2.5

r_rocks <- 2^3
#r_rock # produce : error
#R_rocks # prooduce: error
r_rocks # produce: 8

seq(from = 1, to = 10) # produce:
#[1]  1  2  3  4  5  6  7  8  9 10
seq(1, 10)# produce:
#[1]  1  2  3  4  5  6  7  8  9 10

x <- "hello world"

########################
###
### 2.5 Ejercicios
###
########################

# 1. ¿Por qué no funciona este código?
#my_variable <- 10
#my_varıable 
#> Error: object 'my_varıable' not found
# Porque my_variable no es igual my_varıable

# 2. Modifique cada uno de los siguientes comandos R para que se 
# ejecuten correctamente:

#libary(todyverse)
#
#ggplot(dTA = mpg) + 
#  geom_point(maping = aes(x = displ y = hwy)) +
#  geom_smooth(method = "lm)
# Corrección
library(tidyverse)
#ggplot(data = mpg) +
#  geom_point(mapping = aes(x = displ, y = hwy)) +
#  geom_smooth(method = "lm") # mi solución: incorrecta
# La solución correcta:
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(method = "lm")

# 3. Press Option + Shift + K / Alt + Shift + K. What happens? How 
# can you get to the same place using the menus?
# Alt + Shift + K : Muestra los atajados de teclado. Lo mismo se puede
# acceder yendo a la pestaña Help y dando click en keyboard shortcuts

# 4. Repasemos un ejercicio de la Sección 1.6 . Ejecute las siguientes 
# líneas de código. ¿Cuál de los dos gráficos se guarda como 
# mpg-plot.png? ¿Por qué?

my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)

my_bar_plot
my_scatter_plot

# Se guarda el gráfico de barras porque es el que corresponde a 
# my_bar_plot
