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
library(ggplot2)
library(grid)  # necesario para unit()

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
