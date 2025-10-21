#####################################
###                               ###
###         Capítulo 9            ###
###            Capas              ### 
###                               ###
#####################################

library(tidyverse)

mpg # produce:
# A tibble: 234 × 11
#  manufacturer model     displ  year   cyl trans drv     cty   hwy fl   
#  <chr>        <chr>     <dbl> <int> <int> <chr> <chr> <int> <int> <chr>
#1 audi         a4          1.8  1999     4 auto… f        18    29 p    
#2 audi         a4          1.8  1999     4 manu… f        21    29 p    
#3 audi         a4          2    2008     4 manu… f        20    31 p    
#4 audi         a4          2    2008     4 auto… f        21    30 p    
# NOTA: el marco de datos mpg tiene 234 observaciones o filas sobre
# 38 modelos de automóviles
#View(mpg) # produce: a new window with dataframe

# Entre las variables de mpg tenemos:
# 1. displ: Cilindrada del coche en litros. Variable numérica
# 2. hwy: Eficiencia de combustible de un auto en carretera. En millas
#    por galón (mpg). Un auto con baja eficiencia de combustible 
#    consume más combustible al recorrer la misma distancia queun auto 
#    con alta eficiencia. Variable numérica
# 3. class: tipo de coche, es una variable categórica.

# Relación entre displ y hwy para varios class tipos de autos
ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point()
# Significa que un auto con mayor cilindrada (displ) tiene menor 
# eficiencia de combustible (hwy)
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) + 
  geom_point()
# Cuando class se asigna a shape se obtienen dos advertencias:
# 1: La paleta de formas puede manejar un máximo de 6 valores 
# discretos porque más de 6 se vuelve difícil de discriminar; usted 
# tiene 7.
# 2: Se eliminaron 62 filas que contenían valores faltantes 
# (geom_point()). Son 62 filas que se eliminaron porque contenían
# el séptimo tipo de auto, suv que no se grafica.

# Diagrama de dispersión con puntos de tamaños diferentes
ggplot(mpg, aes(x = displ, y = hwy, size = class)) + 
  geom_point() # produce:
# Using size for a discrete variable is not advised. 
# Diagrama de dispersión con puntos grises de opacidad distinta
ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) + 
  geom_point() # produce:
# Using alpha for a discrete variable is not advised. 

# Lograr que todos los puntos sean azules
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")

########################
###
### 9.2.1 Ejercicios
###
########################

# 1. Crea un diagrama de dispersión de hwyvs  displdonde los puntos 
# estén rellenos de rosa en los triángulos.
ggplot(mpg, aes(x = hwy, y = displ)) + 
  geom_point(shape = "triangle", color = "pink")

# 2. ¿Por qué el siguiente código no generó un gráfico con puntos azules?
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))
# El color específico debe establecerse por fuera del mapeo estético
# Debe ser algo como: 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

# 3. ¿Qué strokehace la estética? ¿Con qué formas funciona? (Pista: 
# usa ?geom_point)
?geom_point
# Parecería que stroke controla el ancho de los puntos
ggplot(mpg, aes(x = displ, y = hwy, stroke = 0.5)) +
  geom_point()

# 4. ¿Qué ocurre si asignas una estética a algo distinto del nombre 
# de una variable, como aes(color = displ < 5)? Ten en cuenta que 
# también deberás especificar x e y.
ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) +
  geom_point()
# Es interesante que si se le asigna una condición a una estética,
# esta estética cambia según la categoría TRUE y FALSE

# Crear un diagrama de dispersión 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
# Crear una linea suave
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
# Crear un diagrama de dispersión y una línea suave
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth()

# Separar las líneas suaves según la variable drv (tipo de tracción)
ggplot(mpg, aes(x = displ, y = hwy, shape = drv)) +
  geom_smooth()
# Lo que resulta un poco extraño

# Separar las líneas suaves según la variable drv (tipo de tracción)
# Y mostrarlas como tiós de líneas diferentes
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) +
  geom_smooth()
# Aquí ya aparece el tipo de transimisión f para transmisión delantera,
# (o front) , r para transmisión trasera, y 4 para transmisión en 
# las 4 llantas

# Se puede clarificar con el siguiente gráfico
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(linetype = drv))
# Esto también sería aceptable
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(aes(shape = drv))
# Pero sería lo mismo que no poner nada en geom_smooth
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth()

# Se puede agrupar e distintas líneas suaves
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(group = drv))
# El problema con la estética group es que no pone leyendas así que
# no se sabe a que nos referimos
#Por eso se podría agrupar con la estética linetype
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(linetype = drv))
# Aquí vemos que ya se pueden ver las leyendas del tipo de tracción
# f, r y 4
# También la estética color es una buena opción si se necesita ver las 
# leyendas:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))

# Mostrar diferentes estéticas en distintas capas
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(
    data = mpg |> filter(class == "2seater" ), # esto es como decir:
           # filter(mpg, class == "2seater")
    color = "red"
  ) + # Hasta aquí se dibuja una capa con puntos rojos para la class
      # "2seater"
  geom_point(
    data = mpg |> filter(class == "2seater" ),
    shape = "circle open", size = 3, color = "red"
  ) # Esta capa pone una capa adicional donde se dibujan unos circulos
    # abiertos (relleno transparente y borde rojo) de 3 pixeles sobre los
    # puntos rojos de la anterior capa

?mpg
# hwy : millas por galón en carretera
# Mostrar histograma para hwy (millas por galón en carretera)
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)
# revela que la distribución del kilometraje en carretera es bimodal
# Mostrar gráfico de densidad para hwy (millas por galón en carretera)
ggplot(mpg, aes(x = hwy)) +
  geom_density()
# revela que la distribución del kilometraje en carretera es bimodal
# Mostrar diagrama de caja para hwy (millas por galón en carretera)
ggplot(mpg,aes(x = hwy)) +
  geom_boxplot()
# revela que hay dos valores atípicos

# Mostrar gráfico de crestas (densidad) según tipo de tracción
library(ggridges)
ggplot(mpg, aes(x = hwy, y = drv, color = drv, fill = drv)) +
  geom_density_ridges(alpha = 0.5)
  #geom_density_ridges(alpha = 0.5, show.legend = FALSE)

########################
###
### 9.3.1 Ejercicios
###
########################

# 1. ¿Qué geometría usarías para dibujar un gráfico de líneas? 
# ¿Un diagrama de caja? ¿Un histograma? ¿Un gráfico de áreas?
# Gráfico de líneas : geom_smooth
# Diagrama de caja : geom_boxplot
# Histrograma : geom_histogram
# gráfico de áreas : geom_density (en las respuestas dice geom_area)

# 2. Anteriormente en este capítulo usamos show.legend sin explicarlo:
ggplot(mpg, aes(x = displ, y = hwy)) +
  #geom_smooth(aes(color = drv), show.legend = FALSE)
  geom_smooth(aes(color = drv))
# ¿Qué show.legend = FALSE hace aquí? ¿Qué pasa si lo eliminas? ¿Por 
# qué crees que lo usamos antes?
# Lo que hace la estética color es crear varias líneas de distintos 
# colores dependiendo del tipo de tracción (drv). Sin embargo por
# defecto el tipo de tracción y su color deben tener una leyeda 
# explicativa. Con show.legend = FALSE  esta leyenda desaparece.
# Si quito show.legend debería aparecer la leyenda con los 3 tipos de 
# tracción. Pienso que se usa para contrastar con la estética group
# que por defecto no muestra las leyendas de las diferente líneas
# que se muestran según el tipo de tracción.

# 3. ¿Qué pretende el se argumento geom_smooth()?
?geom_smooth() # se : Display confidence band around smooth
ggplot(mpg, aes(x = displ, y = hwy)) +
  #geom_smooth(aes(color = drv), show.legend = FALSE)
  geom_smooth(aes(color = drv, se = FALSE)) # asignando FALSE a se,
  # igual sigue mostrando el intervalo de confianza, no entiendo.

# 4. Recree el código R necesario para generar los siguientes 
# gráficos. Tenga en cuenta que siempre que se use una variable 
# categórica en el gráfico, es drv.
# a)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()
# b) 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv))
# c)
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth()
# d)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth()
# e)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(linetype = drv))
# f) 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = "circle", color = "white", size = 5) +
  geom_point(aes(color = drv)) #+
  #geom_point(shape = "circle", color = "white")

# Mostrar el desplazamiento (displ) del motor relacionado con el 
# kilometraje por galón de gasolina en carretera (hwy), en 4 
# gráficos distintos según el número de cilindros 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~cyl) # cyl : número de cilindros
?mpg # cyl : significa número de cilindros

# Facetar con dos variables gracias a facet_grid
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)

# Facetar con dos variables pero liberar la escala (que no sea la 
# misma en x e y). 
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl, scales = "free")

########################
###
### 9.4.1 Ejercicios
###
########################

# 1. ¿Qué pasa si se hace una faceta sobre una variable continua?
# Tal vez tome infinito números de cuadros. Sin embargo en la práctica
# parece que hay una faceta por cada valor único de la variable 
# continua. Así:
ggplot(mpg, aes(x = drv, y = cyl)) + 
  geom_point() +
  facet_wrap(~hwy)

# 2. ¿Qué significan las celdas vacías en el gráfico anterior 
# facet_grid(drv ~ cyl)? Ejecute el siguiente código. 
ggplot(mpg) + 
  geom_point(aes(x = drv, y = cyl))
# ¿Cómo se relacionan con el gráfico resultante?
# Gráfico anterior:
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)
# Significa que no hay, en la data, autos con tracción en las 4
# ruedas y 5 cilindros y tambpoco hay autos con tracción trasera
# de 4 y 5 cilindros.
# Este código
ggplot(mpg) + 
  geom_point(aes(x = drv, y = cyl))
# Muestra que drv y cyl no son variables continuas y todos los
# autos se acumulan en puntos muy específicos dónde se nota claramente
# que no hay autos de 7 cilindros, ni autos con tracción en las 4
# ruedas y 5 cilindros y tambpoco hay autos con tracción trasera
# de 4 y 5 cilindros.

# 3. ¿Qué gráficos crea el siguiente código? ¿Qué . hace?
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .) # Crea 3 facetas horizontales
  #facet_grid(~drv) # Crea 3 facetas verticales (por defecto)
# Crea varias facetas horizontales según el tipo de tracción. No sé
# que hace el punto. Pero el libro de soluciones dice que en este 
# caso el punto (.) significa no gacer facetas entre columnas. 
#
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl) # crea 4 facetas verticales
  #facet_grid(~cyl) # también crea 4 facetas verticales
# Crea varias facetas verticales; el libro de soluciones dice que
# el punto significa no hacer facetas entre filas.
# Es decir, el punto parece que significa mantener todo junto

# 4. Tomemos la primera gráfica facetada de esta sección:
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl, nrow = 2)
# ¿Cuáles son las ventajas de usar facetas en lugar de la estética 
# del color? ¿Cuáles son las desventajas? ¿Cómo podría cambiar el 
# equilibrio si se tuviera un conjunto de datos más grande?
ggplot(mpg, aes(color = cyl)) + 
  geom_point(aes(x = displ, y = hwy))
# El problema es que hay un número espcífico de número de cilindros
# sólo (4, 5, 6 y 8) pero la estética de color pinta como si cyl
# fuera una variable continua (decimales infinitos) pero en la realidad
# cyl es más como una categoría. enotnces la estética de colores
# es un poco confusa, en cambio las facetas son 4 bien limitadas.
# Yo no veo desventajas, pero el libro dice que la desventaja es
# que no se pueden comparar las clases entre si cuando están en 
# gráficos separados.
# Esta es un opción combinada pero igual sigue facetada y no mitiga 
# el problema de que es difícil comparar al mismo tiempo:
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = cyl)) + 
  facet_wrap(~ cyl, nrow = 2)
# La opción del libro es, sin facetas, resaltar solo un valor en cada
# capa. Misolución sería esto:
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "gray") +
  geom_point(
    data = mpg |> filter(cyl == 6),
    color = "red"
  ) +
  geom_point(
    data = mpg |> filter(cyl == 8),
    color = "green"
  ) +
  geom_point(
    data = mpg |> filter(cyl == 4),
    color = "black"
  )+
  geom_point(
    data = mpg |> filter(cyl == 5),
    color = "blue"
  )
# No es una opción elegante porque crece demasiado. Tal vez la solución
# correcta es tranformar el tipo de valor de continuo a categórico,
# a menos que el número de cilindros se necesite para algún cálculo.

# 5. Read ?facet_wrap. What does nrow do? What does ncol do? What 
# other options control the layout of the individual panels? Why 
# doesn’t facet_grid() have nrow and ncol arguments?
?facet_wrap # produce:
#[...]
# nrow, ncol: Number of rows and columns.
# dir : Direction: either "h" for horizontal, the default, or "v", 
# for vertical
#[...]
?facet_grid
# parece que facet_grid sí tiene los argumentos rows y cols pero
# no se usan igual que nrow y ncol porque la grid o grilla se usa
# para mostrar 2 variables entonces ya tiene un ncol y nrow 
# implícitos por el número del tipo de valores que tiene cada una
# de las 2 variables que forman la grilla. En cambio facet_wrap no
# es para 2 variables sino 1 sola

# 6. ¿Cuál de las siguientes gráficas facilita la comparación del
# tamaño del motor ( displ) en coches con diferentes transmisiones? 
# ¿Qué indica esto sobre cuándo colocar una variable de facetado 
# en filas o columnas?
?mpg
# displ: tamaño del motor
# Muestra 3 facetas horizontales
ggplot(mpg, aes(x = displ)) + 
  geom_histogram() + 
  facet_grid(drv ~ .)
# Muestra 3 facetas verticales
ggplot(mpg, aes(x = displ)) + 
  geom_histogram() +
  facet_grid(. ~ drv)
# Es más fácil entender la relación entre el tamaño del motor y 
# el tipo de tracción con el facetado horizontal (drv ~ .) porque
# me permite comparar la cantidad de autos por tipo de transmisión 
# y tracción. Con el facetado vertical sólo es más difícil intuir 
# alguna conclusión.

# 7. Recrea la siguiente gráfica usando facet_wrap()en lugar de 
# facet_grid(). ¿Cómo cambian las posiciones de las etiquetas de 
# las facetas?
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
# facet_grid crea 3 facetas horizontales (posición de las etiquetas)
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(drv ~ .)
# facet_wrap crea tres facetas verticales (posición de las etiquetas)

View(diamonds)
?diamonds
# cut : calidad del corte
# En el eje x muestra la calidad del corte, y en el eje Y muestra
# count. Sin embargo count no es una variable de diamonds, de dónde 
# viene?
ggplot(diamonds, aes(x = cut)) +
  geom_bar()
# Viene de:
# - Los gráficos de barras, histogramas y polígonos de frecuencia 
# agrupan sus datos y luego trazan los recuentos de contenedores, 
# la cantidad de puntos que caen en cada contenedor.
# - Los suavizadores ajustan un modelo a sus datos y luego trazan 
# predicciones a partir del modelo.
# - Los diagramas de caja calculan el resumen de cinco números de la 
# distribución y luego muestran ese resumen como un cuadro con 
# formato especial.
# En resumen, cuando un gráfico no recibe las varibles necesarias,
# es como que tomaría unas variables por defecto para completar
# un gráfico.
# Por ejemplo en geom_bar el valor predeterminado de stat es "count",
# y cada geom tiene una estadística predeterminada
?geom_bar

# Muestra un gráfico de barras en el que se asigna a la altura de 
# las barras el valor de la variable Y, donde n son los valores sin
# procesar de la variable Y.
diamonds |> 
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

# Muestra un gráfico de barras de proporciones en vez de recuentos.
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

?stat_summary
# Muestra los valores mínimos, máximos y promedios de la profundidad
# (depth) de los diamantes según su cut (o calidad de corte)
ggplot(diamonds) +
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )
# stat_summary : resume los valores de y para cada valor único de x.

########################
###
### 9.5.1 Ejercicios
###
########################

# 1. ¿Cuál es la función geom predeterminada asociada a 
# stat_summary()[?]? ¿Cómo se podría reescribir el gráfico 
# anterior para usar esa función geom en lugar de la función stat?
?stat_summary()
# pararece que para stat_summary el tipo de geom es pointrange
# Esta es mi versión
ggplot(diamonds, aes(x = cut, y = depth)) + 
  geom_pointrange(aes(ymin = min(depth), ymax = max(depth)))
?geom_pointrange()
# Pero la versión del libro es esta:
diamonds |>
  group_by(cut) |>
  summarize(
    lower = min(depth),
    upper = max(depth),
    midpoint = median(depth)
  ) |>
  ggplot(aes(x = cut, y = midpoint)) +
  geom_pointrange(aes(ymin = lower, ymax = upper))

# 2. ¿Qué geom_col()hace? ¿En qué se diferencia de geom_bar()...?
# geom_col debe tener en su estética un x y un y
ggplot(diamonds, aes(x = cut, y = depth)) + 
  geom_col()
# No funciona con un valor y, geom_bar sólo debe tener en su estética 
# un x
ggplot(diamonds, aes(x = cut)) + 
  geom_bar()
# No funciona
ggplot(diamonds, aes(x = cut)) + 
  geom_col()
# NO funciona
ggplot(diamonds, aes(x = cut, y = depth)) + 
  geom_bar()
?geom_bar()

# 3. La mayoría de las geometrías y estadísticas vienen en pares 
# que casi siempre se usan en conjunto. Haz una lista de todos los 
# pares. ¿Qué tienen en común? (Pista: Lee la documentación).
# ?geom_bar() : stat_count()
# ?geom_boxplot() : stat_boxplot()
# ?geom_density() : stat_density()
# ?geom_smooth() : stat_smooth()

# 4. ¿Qué variables componen stat_smooth() el cálculo? ¿Qué 
# argumentos controlan su comportamiento?
?stat_smooth()
# after_stat(x), after_stat(y), after_stat(xmin), after_stat(ymin), 
# after_stat(xmax), after_stat(ymax), after_stat(se) standard error

# 5. En nuestro gráfico de barras de proporciones, necesitábamos 
# establecer group = 1. ¿Por qué? En otras palabras, ¿cuál es el 
# problema con estos dos gráficos?
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop))) + 
  geom_bar()
# Muestra un gráfico de barras de proporciones en vez de recuentos.
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()
# Qué pasaría si les doy el argumento group = 1
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop), 
                     group =  color)) + 
  geom_bar()
# No entiendo bien pero parece que sin group se llena todo el gráfico
# de barras, por lo que siempre hay que agrupar

# Gráfico de barras con el borde coloreado
ggplot(mpg, aes(x = drv, color = drv)) +
  geom_bar()
# Gráfico de barras coloreadas
ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar()

# Gráfico de barras multicolor (en cada barra) según el tipo de auto
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar()

# Gráfico de barras con colores transparentes según el tipo de auto
# gracias al argumento position que toma "identity" como valor 
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(alpha = 1/5,  position = "identity")
# Gráfico de barras con bordes coloreados según el tipo de auto
# gracias al argumento "identity"
ggplot(mpg, aes(x = drv, color = class)) +
  geom_bar(fill = NA, position = "identity")
# NOTA: A mi e parecen confusos ambos gráfico, prefiero el que se generó
# por defecto sin "identity"

# Muestra un gráfico con barras apiladas que tienen la misma altura
# lo que facilita la comparación de proporciones entre grupos gracias
# al argumento "fill".
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "fill")
# Muestra un gráfico de barras superpuestas una a lado de otra, lo 
# que facilita comparar valores individuales gracias al argumento
# "dodge"
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar(position = "dodge")

# Los puntos de los diagramas de dispersión suelen superponerse porque
# los números de las posiciones se redondean. Así un diagrama de 
# dispersión con 234 observaciones puede llegar a tener sólo 126 
# puntos. Este problemase conoce como sobregraficación. Esto dificulta
# la visualización de la distribución de los datos
# Esto se soluciona con el ajuste de posición "jitter" pues añade cierta
# cantidad de ruido aleatorio a cada punto (lo que hace que los puntos)
# tengan posiciones aleatorias ligeramente diferentes para que se 
# diferencien en el diagrama de dispersión.
?mpg
# Muestra un diagrama de dispersión con el tamaño del motor (displ)
# en el eje x y la cantidad de millas que recorre por galón en
# carretera (hwy) en el eje Y
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")
# Muestra un diagrama de dispersión con el tamaño del motor (displ)
# en el eje x y la cantidad de millas que recorre por galón en
# carretera (hwy) en el eje Y. Coloreado según el tipo de auto
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(position = "jitter")

########################
###
### 9.6.1 Ejercicios
###
########################

# ¿Cuál es el problema con la siguiente gráfica? ¿Cómo podrías 
# mejorarla?
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point()
#View(mpg)
# Me parece que pueden estar puntos sobrepuestos así que darles un poco
# de ruido aleatorio podría ser conveniente.
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")

# 2. ¿Cuál es, si acaso, la diferencia entre ambas tramas? ¿Por qué?
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")
# NO hay diferencia

# 3. ¿Qué parámetros para geom_jitter() controlar la cantidad de 
# jittering?
?geom_jitter() # Parece que width y height

# 4. Comparar y contrastar geom_jitter()con geom_count().
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point(position = "jitter")
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_jitter() # De inicio hay diferencia pero mínima
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0.9) # Es notorio que hay más ruido
ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_jitter(width = 0.9, height = 0.9) # Parece que los puntos 
# están menos apiñado

# 5. ¿Cuál es el ajuste de posición predeterminado para 
# geom_boxplot()[?]? Crea una visualización del mpgconjunto de 
# datos que lo muestre.
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_boxplot() # boxplot es una caja
?geom_boxplot() # el valor predeterminado del argumento position de 
# geom_boxplot() es "dodge2". Este código lo demuestra:
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_boxplot(position = "dodge2")
# Este último gráfico y el anterior, demeuestra que el valor de 
# por defecto de position es "dodge2"

nz <- map_data("nz")
# Produce un mapa alargado horizontalmente
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")
# Produce un mapa aparentemente normal
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

# clear es la claridad (o calidad) de un diamante
bar <- ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = clarity, fill = clarity),
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1)
# Muestra un clásico gráfico de barras coloreadas según claridad 
# del diamante
bar
# Muestra el mismo gráfico de barras pero horizontales
bar + coord_flip()
# Muestra un gráfico Coxcomb (que no se qué significa)
bar + coord_polar() # Es como un caracol donde las barras se 
# representan como parte de un caracol

########################
###
### 9.7.1 Ejercicios
###
########################

# 1. Convierta un gráfico de barras apiladas en un gráfico circular 
# usando coord_polar().
ggplot(mpg, aes(x = class, fill = class)) +
  geom_bar() +
  coord_polar()

# 2. ¿Cuál es la diferencia entre coord_quickmap()y coord_map()?
# Mi parecer es que coord_map crea un gráfico alargado en lo horizontal
# mientras que coord_quickmap crea un gráfico real

# 3. ¿Qué te dice el siguiente gráfico sobre la relación entre el 
# consumo de combustible en ciudad y en carretera? ¿Por qué es 
# coord_fixed()importante? ¿Qué función geom_abline()cumple?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()
?geom_abline()
# La relación entre el consumo de combustible en ciudad y en carretera
# es directamente proporcional. abline() crea una línea de referencia
# con una pendiente que más o menos nos da una idea de la tendencia
# de una relación. coord_fixed crea un sistema de cuadrículas donde
# x e y tienen el mismo tamaño (es decir si un cm es 20 pixeles en x,
# un cm también es 20 pixeles en y); tomando en cuenta esto, abline 
# crea una línea dónde x e y son iguales lo que evidencia que todos
# los autos avanzan más kilómetros en carretera que en ciudad.

# Así se vería una plantilla de gráficos con ggplot (hasta ahora)
# agregando ajustes de posición, estadísticas, sistemas de coordenadas
# y facetas
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
