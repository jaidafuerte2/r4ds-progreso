#####################################
###                               ###
###         Capítulo 11           ###
###        Comunicación           ###
###                               ###
#####################################

library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

df <- tibble(
  x = 1:10,
  y = cumsum(x^2)
)
ggplot(df, aes(x, y)) +
  geom_point()
# Cambiar las cadenas de texto a ecuaciones matemáticas con la función
# quote()
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(x[i]),
    y = quote(sum(x[i] ^ 2, i == 1, n))
  )

########################
###
### 11.2.1 Ejercicios
###
########################

# 1. Create one plot on the fuel economy data with customized title, 
# subtitle, caption, x, y, and color labels.

ggplot(mpg, aes(x = hwy, fill = drv)) +
  geom_bar() +
  labs(
    title = "Los autos con tracción delantera consumen menos gasolina",
    subtitle = "Los menos eficientes tienen tracción en 4 ruedas",
    x = "Kilómetros recorriddos por galón",
    y = "cantidad de autos",
    color = "tipo de tracción",
    caption = "nada que agregar"
  )

# 2. Recree el siguiente gráfico con los datos de consumo de 
# combustible. Tenga en cuenta que tanto el color como la forma de 
# los puntos varían según el tipo de transmisión.
ggplot(mpg, aes(x = cty, y = hwy, shape = factor(drv))) +
  geom_point(aes(color = drv)) +
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train"
  )

# 3. Tome un gráfico exploratorio que haya creado en el último mes 
# y agréguele títulos informativos para que sea más fácil de 
# entender para otros.
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1))) +
  labs(
    title = "Los diamantes de mayor peso cuestan más",
    subtitle = "Aunque hay variaciones",
    x = "peso",
    y = " precio"
  )

# Crea una tabla con los mayores motores en cada tipo de tracción
label_info <- mpg |>
  group_by(drv) |> # se agrupa por tracción (drv)
  arrange(desc(displ)) |> # Esto ordena en descendente
  slice_head(n = 1) |> # se escoge sólo uno (el mayor)
  mutate(
    drive_type = case_when( # se crea una nueva variable drive_type
      drv == "f" ~ "front-wheel drive",
      drv == "r" ~ "rear-wheel drive",
      drv == "4" ~ "4-wheel drive"
    )
  ) |>
  select(displ, hwy, drv, drive_type) # son las variables que se muestran

label_info # Produce:
# A tibble: 3 × 4
# Groups:   drv [3]
#  displ   hwy drv   drive_type       
#  <dbl> <int> <chr> <chr>            
#1   6.5    17 4     4-wheel drive    
#2   5.3    25 f     front-wheel drive
#3   7      24 r     rear-wheel drive 

# Se crea un diagrama de dispersión con las leyendas drive_type
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) + # con transparencia
  geom_smooth(se = FALSE) + # sin error standar 
  geom_text(
    data = label_info, # Aquí va la nueva tabla
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) +
  theme(legend.position = "none") # desactiva todas las leyendas

# geom_label_repel() ajusta automaticamente las etiquetas para que 
# no se superpongan 
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  ggrepel::geom_label_repel(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2
  ) +
  theme(legend.position = "none")

# Selecciono los valores atípicos potenciales que recorren más de 40 
# o más de 20 siempre que el motor sea mayor a 5.0
potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))
potential_outliers # produce:
# A tibble: 9 × 11
#  manufacturer model      displ  year   cyl trans drv     cty   hwy fl   
#  <chr>        <chr>      <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#1 chevrolet    corvette     5.7  1999     8 manu… r        16    26 p    
#2 chevrolet    corvette     5.7  1999     8 auto… r        15    23 p    
#3 chevrolet    corvette     6.2  2008     8 manu… r        16    26 p    
#4 chevrolet    corvette     6.2  2008     8 auto… r        15    25 p 

# Muestra un diagrama de dispersión con los textos de los modelos
# con potenciales valores atípicos para hwy y disp
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  # geom_text_repel usa dos parámetros: data que son los valores de la
  # base de datos a los que se aplicará un texto; y la estética que
  # indica cual texto se podrá en los valores de la base de datos
  ggrepel::geom_text_repel(data = potential_outliers, aes(label = model)) +
  # Crea un círculo rojo en los potenciales valores atípicos
  geom_point(data = potential_outliers, color = "red") +
  geom_point(
    data = potential_outliers,
    # Crea un círculo rojo dentro de los círculos rojos
    color = "red", size = 3, shape = "circle open"
  )

# Se crea un texto y se usa str_wrap para poner un salto de línea
# al llegar al caracter 30
trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 30)
trend_text # Produce:
"Larger engine sizes tend to\nhave lower fuel economy."
# Crea un diagrama de dispersión que muestra que los carros con 
# grandes motores consumen más gasolina pues recorren menos kms por
# galón. En este gráfico se crean anotaciones con la función annotate
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 38, # parece que"label" se refiere a texto
    label = trend_text, # Este es el texto que ya creé antes
    hjust = "left", color = "red"
  ) +
  annotate(
    geom = "segment", # parece que "segment" se refiere a gráficos
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed")
  )
# Mi versión
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 4, y = 38, # parece que"label" se refiere a texto
    label = "hola mundo", # Este es el texto que ya creé antes
    hjust = "right", color = "green"
  ) +
  annotate(
    geom = "segment", # parece que "segment" se refiere a gráficos
    x = 1.75, y = 39, xend = 5, yend = 25, color = "blue",
    arrow = arrow(type = "closed")
  )

########################
###
### 11.3.1 Ejercicios
###
########################

# 1. Use geom_text() with infinite positions to place text at the four corners 
# of the plot.
ggplot(mpg, aes(cty, hwy)) +
  geom_point()
#
ggplot(mpg, aes(cty, hwy)) +
  geom_point() +
  theme_minimal() # cambia el fondo gris por blanco
# Muestra un diagrama de dispersión relacionando cty y hwy
p <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(alpha = 0.6) +
  labs(title = "City vs Highway MPG") +
  theme_minimal() # Es para cambiar el fondo gris a blanco
# Añadimos textos en las 4 esquinas usando -Inf / Inf. -Inf se refiere
# a abajo y a la izquierda. Inf se refiere a arriba y a la derecha
p +
  geom_text(aes(x = -Inf, y = -Inf), label = "bottom-left", 
            hjust = -0.1, vjust = -0.1, size = 3.5) +
  geom_text(aes(x =  Inf, y = -Inf), label = "bottom-right", 
            hjust =  1.1, vjust = -0.1, size = 3.5) +
  geom_text(aes(x = -Inf, y =  Inf), label = "top-left", 
            hjust = -0.1, vjust =  1.1, size = 3.5) +
  geom_text(aes(x =  Inf, y =  Inf), label = "top-right", 
            hjust =  1.1, vjust =  1.1, size = 3.5)

# 2. Úsalo annotate()para añadir un punto geométrico en medio de 
# tu último gráfico sin tener que crear un tibble. Personaliza la 
# forma, el tamaño o el color del punto.
library(ggplot2)

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(alpha = 0.6) +
  annotate(
    geom = "point", # usa la geometría punto
    x = mean(mpg$cty), # calcula el promedio de la columna cty
    y = mean(mpg$hwy), # calcula el promedio de la columna hwy
    color = "red",
    shape = 8,   # forma: estrella
    size = 5     # tamaño del punto
  ) +
  theme_minimal()

# 3. ¿Cómo geom_text()interactúan las etiquetas con el facetado? 
# ¿Cómo se puede añadir una etiqueta a una sola faceta? ¿Cómo se 
# puede poner una etiqueta diferente en cada faceta? (Pista: Piensa 
# en el conjunto de datos que se está pasando a geom_text()).
# Crea 3 facetas, una para cada tipo de tracción.
p <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ drv) +
  theme_minimal()
# Muestra 3 facetas
p
# # Muesta una etiqueta con el modelo de cada auto
p + geom_text(aes(label = model), size = 2, vjust = -0.5)
#
# Un tibble con una sola fila (solo facet "4")
label_data <- data.frame(
  drv = "4",
  displ = 6,
  hwy = 40,
  label = "Etiqueta solo en 4WD"
)
#label_data
# Muestra una etiqueta solo en la faceta de drv = "4"
p + 
  geom_text(data = label_data, aes(x = displ, y = hwy, label = label), 
            color = "red", size = 3)
# Entonces para poner un texto distinto en cada facet, creo un 
# pequeño dataset con una fila por cada drv. Así:
label_data2 <- data.frame(
  drv = c("f", "r", "4"),
  displ = c(2, 5, 6),
  hwy = c(40, 20, 30),
  label = c("Front", "Rear", "4WD")
)

p + 
  geom_text(data = label_data2, aes(x = displ, y = hwy, label = label), 
            color = "blue", size = 3)

# 4. ¿Qué argumentos para geom_label() controlar la apariencia del 
# cuadro de fondo?
?geom_label() # Creo que label.padding
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_label(aes(label = drv), 
             size = 3, 
             fill = "yellow", 
             color = "blue",
             label.size = 0.5, # ancho del borde
             label.r = unit(0.3, "lines"), # redondea bordes
             label.padding = unit(0.25, "lines") # espacio interior
             )

# 5. What are the four arguments to arrow()? How do they work? 
# Create a series of plots that demonstrate the most important 
# options.
?arrow()
df <- data.frame(x = 1, y = 1, xend = 5, yend = 5)
df # produce:
#  x y xend yend
#1 1 1    5    5
# Dibuja una línea diagonal con una flecha al final
ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow()) +
  theme_minimal()
# Cambiar el ángulo de la punta
ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(angle = 15))
# Alarga la punta de la flecha
ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.8, "inches")))
# Mostrar flechas en ambos extremos de la punta
ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(ends = "both"))
# Mostrar la punta rellena con "closed". "open" es predeterminado
# sólo para contorno
ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(type = "closed"))

# Relaciona el tamaño del motor con la eficiencia del combustible
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point()
# Cambia las escalas del eje x e y y también las etiquetas de la escala 
# de color de la derecha
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 45, by = 5)) +# breaks anula la
  # opción predeterminada
  scale_x_continuous(breaks = seq(0, 7, by = 0.5)) +
  scale_color_discrete(labels = c("4" = "4-wheel", "f" = "front", 
                                  "r" = "rear"))

# Relaciona el precio con el corte del diamante
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05)
# Muestra signo de dolar en la escala de precios
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(labels = scales::label_dollar()) # le pone signo
  # de dólar a las escalas de x (del precio)
# Cambia las escalas a un sólo dígito por cada mil, cambia el rango
# y los intervalos de la escala en el eje X
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"), 
    breaks = seq(1000, 19000, by = 6000)
  )

# Muestra barras de porcentajes (gracias a position = "fill") para
# cada tipo de corte de diamante
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill")
# Cambia las relaciones /100 a porcentajes en la escala del eje Y
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = label_percent())

# Muestra cuando inició el gobierno de un presidente
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point()
# Muestra cuando inició y cuando terminó el gobierno de una presidente
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id))
# Puedo usar breaks para mostrar con precisión donde ocurren las 
# observaciones y date_labels para dar formato al año. Esto se hace
# cuando hay pocos datos
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(name = NULL, breaks = presidential$start, 
               date_labels = "'%y")

# Grafico de dispersión que compara el tamaño del motor con la 
# eficiciencia del combustible y coloreada por tipo de auto
base <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))
base + theme(legend.position = "right") # predeterminada
base + theme(legend.position = "left")
# El tipo de auto va arriba
base + 
  theme(legend.position = "top")
# El tipo de auto va arriba  pero en tres filas
base + 
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 3))
# El tipo de auto va abajo en 3 filas
base + 
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3))

# Mustra un diagrama de dispersión con una línea suave que relaciona
# el tamaño del motor con la eficiencia del combustible y coloreado
# por tipo de auto
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE)
# Poner el tió de auto abajo:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")
# Las bolitas de las leyendas del tipo de auto (que muestran 
# los colores) se harían un poco más grandes. Tal vez ayude a 
# ver los colores de los círculos
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, 
                              override.aes = list(size = 4)))
# Parece que nrow = 2 no hace nada así que mejor lo quito
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Muestra la variación del precio por el peso del diamante
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d()
ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_point(aes(color = cut))+
  geom_smooth(se = FALSE)
# Muestra la variación del precio por el peso del diamante aplicando
# una transformación logarítmica. Lo que resulta en que se puede ver
# más fácil la relación. Pero se daña las leyendas de las escalas 
# X e Y
ggplot(diamonds, aes(x = log10(carat), y = log10(price))) +
  geom_bin2d()
# Soluciono  el daño a las leyendas de las escalas , etiquetando los
# ejes en la escala original de los datos
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()

# Muestra la relación entre el tamaño del motor y la eficiencia del
# combustible. Coloreada por tipo de auto 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv))
# Cambio de color para daltónicos con scale_color_brewer():
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")
# Cambio de color para daltónicos con scale_color_brewer(). Además
# Puedo usar shape() para darles forma, además de color, a los puntos
# que representan el tipo de auto
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

# Asignar manualmente los colores para ciertos valores con rgb 
# hexadecimal cy la función scale_color_manual()
View(presidential)
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "#E81B23", 
                                Democratic = "#00AEF3"))

# Relación entre tamaño del motor y eficiencia del combustible
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()
# Filtrando la relación se obtiene un gráfico en el que se afectó
# la suavidad de la curva y las escalas de los ejes X e Y (parece que
# hay una gran banda de error estandar)
mpg |>
  filter(displ >= 5 & displ <= 6 & hwy >= 10 & hwy <= 25) |>
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()
# Aquí sigue afectada la suavidad de la curva
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  scale_x_continuous(limits = c(5, 6)) +
  scale_y_continuous(limits = c(10, 25))
# Para mejorar la suavidad de la curva se podrían poner los límites 
# en la función coord_cartesian()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 6), ylim = c(10, 25))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv, shape = drv))
# Creo dos nuesvas tablas con sólo los tipos de auto "suv" y "compact"
suv <- mpg |> filter(class == "suv")
compact <- mpg |> filter(class == "compact")
# Relaciono el tamaño del motor con la eficiencia de combustible
# de los suv y coloreo por tipo de tracción
ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point()
# Relaciono el tamaño del motor con la eficiencia de combustible
# de los compact y coloreo por tipo de tracción
ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point()
# Se comparten las escalas de los dos gráficos , entrenando las escalas
# con los limits de datos completos
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))
# Mostrar un diagrama de flujo con las escalas entrenadas
ggplot(suv, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
# Mostrar un diagrama de flujo con las escalas entrenadas
ggplot(compact, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

########################
###
### 11.4.1 Ejercicios
###
########################

# 1. ¿Por qué el siguiente código no anula la escala predeterminada?

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_color_gradient(low = "white", high = "red") +
  coord_fixed()
# Porque lo correcto no es usar scale_color_gradient() sino
# scale_fill_viridis()
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  #scale_color_gradient(low = "white", high = "red") +
  scale_fill_viridis_c() # c para continuos
# Para mostrar paletas agrupadas se usa _b
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed() +
  #scale_color_gradient(low = "white", high = "red") +
  scale_fill_viridis_b() # b para paletas agrupadas

# 2. ¿Cuál es el primer argumento para cada escala? ¿Cómo se 
# compara con labs()...?
?scale_fill_viridis_b() # El primer argumento es name, el nombre de 
# la escala. Por defecto es waver que toma el nombre del primer
# mapeado usado por la estética (sería para los anteriores ejemplos
# X e Y)
?labs() # Se observa que tiene muchas más etiquetas de texto: title,
# subtitle, caption, tag, dictionary, alt y alt_insight y todo
# con valores por defecto waiver

# 3. Modificar la visualización de los mandatos presidenciales de la 
# siguiente manera:
#   
# - Combinando las dos variantes que personalizan los colores y 
#   los cortes del eje x.
# - Mejorar la visualización del eje y.
# - Etiquetando cada mandato con el nombre del presidente.
# - Añadiendo etiquetas informativas a los gráficos.
# - Establecer pausas cada 4 años (¡esto es más complicado de lo 
#   que parece!).
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(name = NULL, breaks = presidential$start, 
               date_labels = "'%y") +
  scale_color_manual(values = c(Republican = "#E81B23", 
                                Democratic = "#00AEF3")) +
  geom_text(aes(label = name), hjust = -0.1, size = 3) + 
  geom_label(aes(label = paste0(year(start), "-", year(end))),
             hjust = 1.2, size = 2.8, label.size = 0, fill = "white", 
             alpha = 0.6) +
  labs(title = "Presidentes de EEUU", x = "Años de gobierno", 
       y = "Identificación")

# 4. Primero, crea el siguiente gráfico. Luego, modifica el 
# código override.aes para que la leyenda sea más fácil de leer.
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20)
?override.aes
# El problema con la primera versión del libro es que los puntos
# de colores de la leyenda casai no se ven por la transparencia alpha.
# que hereda la transparencia del gráfico. Así que una opción puede
# ser guide_legend() con override.aes
# La nueva versión sería algo así:
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) +
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1, size = 8) # alpha = 1 significa
      # sin trasnaprencia, size es el tamaño de los círculos
    )
  )



