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
#
df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month # produce:
#> [1] Jan Feb Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

# gss_cat o forcats::gss_cat es una muestra de datos de la Encuesta
# Social General, una encuesta Estadounidense realizada por la 
# Universidad de Chicago
gss_cat # produce:
# A tibble: 21,483 × 9
#   year marital         age race  rincome    partyid relig denom tvhours
#  <int> <fct>         <int> <fct> <fct>      <fct>   <fct> <fct>   <int>
#1  2000 Never married    26 White $8000 to … Ind,ne… Prot… Sout…      12
#2  2000 Divorced         48 White $8000 to … Not st… Prot… Bapt…      NA
#3  2000 Widowed          67 White Not appli… Indepe… Prot… No d…       2
#4  2000 Never married    39 White Not appli… Ind,ne… Orth… Not …       4

#View(gss_cat) # esto no funciona, lo crrecto es View(forcats::gss_cat)
# Para ver los niveles de los factores fácilmente se puede usar count()
gss_cat |>
  count(race) # produce:
# A tibble: 3 × 2
#  race      n
#  <fct> <int>
#1 Other  1959
#2 Black  3129
#3 White 16395

########################
###
### 16.3.1 Ejercicios
###
########################
View(forcats::gss_cat)

# 1. Explora la distribución de rincome(ingresos declarados). 
# ¿Qué hace que el gráfico de barras predeterminado sea difícil 
# de entender? ¿Cómo podrías mejorar el gráfico?
gss_cat %>% 
  ggplot(aes(x = rincome)) +
  geom_bar()
# El gráfico es difícil de entender porque hay un montón de 
# categorías y no se entiende, en el gráfico, de qué va cada 
# categoría, las etiquetas ocupan demasiado espacio en la 
# horizontal y sería mejor rotarlas. Además las categrías sin 
# un orden numérico rompen la interpretación. En un ingreso se
# podría esperar una especie de escala pero rincome de gss_cat es
# un factor no ordenado
# 
# Cómo mejorar el gráfico?
# 1. Reordenar las categorías por frecuencia
gss_cat %>% 
  ggplot(aes(x = fct_infreq(rincome))) +
  geom_bar() +
  coord_flip() # cambia los ejes para poder leer las etiquetas 
  # de rincome
# 2. Reordenar por valor numérico de los ingresos
# Crear un factor ordenado:
gss_cat %>% 
  mutate(rincome = fct_relevel(
    rincome,
    "Not applicable",
    "Don't know",
    "Refused",
    "No answer",
    "$1000 to 2999",
    "$3000 to 3999",
    "$4000 to 4999",
    "$5000 to 5999",
    "$6000 to 6999",
    "$7000 to 7999",
    "$8000 to 9999",
    "$10000 or more"
  )) %>% 
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip() # cambia los ejes

# 2. ¿Cuál es la religión mas común en esta encuesta? ¿Qué es lo más 
# común partyid?
#
# Contar las ocurrencias de cada nivel de 'relig', ordenar y 
# mostrar el top 1
gss_cat %>%
  count(relig) %>%
  arrange(desc(n))
# A tibble: 15 × 2
#  relig                       n
#  <fct>                   <int>
#1 Protestant              10846
#2 Catholic                 5124
#3 None                     3523
#4 Christian                 689
#
# Contar las ocurrencias de cada nivel de 'partyid', ordenar y 
# mostrar el top 1
gss_cat %>%
  count(partyid) %>%
  arrange(desc(n))
# A tibble: 10 × 2
#  partyid                n
#  <fct>              <int>
#1 Independent         4119
#2 Not str democrat    3690
#3 Strong democrat     3490
#4 Not str republican  3032

# 3. ¿A cuál relig se aplica denom (la denominación)? ¿Cómo se puede 
# averiguar con una tabla? ¿Cómo se puede averiguar con una 
# visualización?
#
# denom aplica sólo para los protestantes
#
gss_cat %>% 
  count(relig, denom) # produce:
# A tibble: 47 × 3
#  relig                   denom               n
#  <fct>                   <fct>           <int>
#1 No answer               No answer          93
#2 Don't know              Not applicable     15
#3 Inter-nondenominational Not applicable    109
#4 Native american         Not applicable     23
#
# El gráfico es un poco confuso, es un ejercicio difícil porque
# se esperaría que en las religiones no protestantes el valor de
# denom se NA pero no es así, hay una gama de valores no aplicables

# Número de horas promedio que dedican a ver tv por día en las 
# diferentes religiones
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
relig_summary # produce:
# A tibble: 15 × 3
#  relig                   tvhours     n
#  <fct>                     <dbl> <int>
#1 No answer                  2.72    93
#2 Don't know                 4.62    15
#3 Inter-nondenominational    2.87   109
#4 Native american            3.46    23
# NOTA: Es interesante que los nativos americanos son los que ven 
# más televisión
#
# Gráfico que muestra claramente que los nativos americanos son los 
# que ven más tv
ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

# Para interpretar mejor este gráfico, se puede reordenar los niveles
# usando fct_reorder()
ggplot(relig_summary, aes(x = tvhours, 
                          y = fct_reorder(relig, tvhours))) +
  geom_point()
# NOTA: Los hinuístas son los que ven menos tv, esto también es 
# interesante

# Mutar reordenando la religión por horas que se ve tv
relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()
#
relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) # produce:
# A tibble: 15 × 3
#  relig                   tvhours     n
#  <fct>                     <dbl> <int>
#1 No answer                  2.72    93
#2 Don't know                 4.62    15
#3 Inter-nondenominational    2.87   109
#4 Native american            3.46    23
# NOTA: La función sólo reordena internamente el orden de los
# niveles del factor, por eso no se ve la tabla ordenada por
# horas que se ve la tv

# Crear un gráfico para observar como varía la edad promedio según 
# el nivel de ingresos declarado.
rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()
# NOTA: No es adecuado usar fct_reorder en este caso porque rincome 
# ya tiene  un orden establecido.

# Sin embargo se puede usar fct_relevel() para mover "Not applicable"
# al principio

# También se puede usar fct_reorder2() para recolorear
by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )
by_age # produce:
# A tibble: 351 × 4
# Groups:   age [72]
#    age marital           n    prop
#  <int> <fct>         <int>   <dbl>
#1    18 Never married    89 0.978  
#2    18 Married           2 0.0220 
#3    19 Never married   234 0.940  
#4    19 Divorced          3 0.0120 
#
# Relacionar los ejes con el estado civil
ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")
# Relacionar sólo edad y proporción
ggplot(by_age, aes(x = age, y = prop)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")
#
# Relacionar los ejes con estado civil
ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")
# Relacionar sólo edad y proporción
ggplot(by_age, aes(x = age, y = prop )) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")
# Realmente no me parece que hay mayor diferencia en el uso de 
# colores

# fct_infreq() ordena los niveles en frecuencia decreciente y
# fct_rev() ordena en frecuencia creciente para que en el gráfico,
# los valores más altos aparezcan a la derecha.
# Mostrar un gráfico de barras para el estado civil.
gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

########################
###
### 15.3.5 Ejercicios
###
########################

# 1. Hay algunos números sospechosamente altos en tvhours. ¿Es la 
# media un buen resumen?
# Cuando hay valores extremos , la media no es un buen resumen porque
# es muy sensible a estos valores atípicos. Lo mejor es usar la mediana
# porque respeta la forma de la distribución.

# 2. Para cada factor, gss_catidentifique si el orden de los niveles 
# es arbitrario o basado en principios.
# a) marital: primero los que no contestaron, luego los no casados,
# al final los casados, parece lógico
# b) relig: no parece haber un orden natural o lógico, al menos
# a simple vista sin ser experto en religiones
# c) denom es un orden arbitrario del tipo de protestantes.
# d) race: blanco, negro, otros -> parece orden arbitrario
# e) rincome: en general parece que los niveles sí están ordenados
# por cantidad de ganancias declaradas.
# f) partyid: parece que están ordenados por partidos desde los
# extremos pasando por el centro o independientes hasta llegar 
# al otro extremo del espectro político
# degree: si tiene un orden educativo o por escolaridad

# 3. ¿Por qué al mover la opción “No aplicable” al principio de los 
# niveles, esta pasó a la parte inferior del gráfico?
# Porque ggplot ordena las barras según los niveles de los factores
# pero en las coordenadas verticales el principio aparece abajo.
# Entonces si "Not applicable" queda como primer nivel del factor
# entonces ggplot lo dibuja primero, es decir abajo