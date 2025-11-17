#####################################
###                               ###
###         Capítulo 16           ###
###           Factores            ###
###         (categorías)          ###
###                               ###
#####################################

library(tidyverse)

# Los factores se usan para variables categóricas, es decir, variables
# que tienen un conjunto fijo y conocido de valores posibles.

# Para crear un factor, primero se debe crear una lista de los niveles
# válidos:
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
# Crear un par de vectores
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
# Crear un factor
y1 <- factor(x1, levels = month_levels) # Un factor es un tipo de
# dato asociado a un conjunto finito de cadenas
y1 # produce:
#> [1] Dec Apr Jan Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#
sort(y1) # produce:
#> [1] Jan Mar Apr Dec
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

# Crear un factor con x2
y2 <- factor(x2, levels = month_levels)
y2 # produce:
#> [1] Dec  Apr  <NA> Mar # Los valores que no están dentro del nivel
#>                        # se convierten en NA
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#
# NOTA: Tal vez esto último no es tan bueno y sea preferible usar
# fct() porque todas las cadenas de x2 deben estar entre los niveles
# que se están dando
y2 <- fct(x2, levels = month_levels) # produce:
#> Error in `fct()`:
#> ! All values of `x` must appear in `levels` or `na`
#> ℹ Missing level: "Jam"

# Si se omiten los niveles o categorías, se tomarán los datos 
# en orden alfabético:
factor(x1)
#> [1] Dec Apr Jan Mar
#> Levels: Apr Dec Jan Mar

# Acceder al conjunto de niveles válido:
levels(y2) # produce:
#[1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
#[12] "Dec"

# También se puede crear un factor con la función col_factor()
csv <- "
month,value
Jan,12
Feb,56
Mar,12"

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month # produce:
#> [1] Jan Feb Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

