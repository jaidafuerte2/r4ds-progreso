#####################################
###                               ###
###         Capítulo 1            ###
### Visualización de Datos        ### 
###                               ###
#####################################

library(tidyverse)
library(palmerpenguins)
library(ggthemes)

penguins # produce: 
## A tibble: 344 × 8
#   species island    bill_length_mm bill_depth_mm flipper_length_mm
#   <fct>   <fct>              <dbl>         <dbl>             <int>
#1 Adelie  Torgersen           39.1          18.7               181
#2 Adelie  Torgersen           39.5          17.4               186
#3 Adelie  Torgersen           40.3          18                 195
#4 Adelie  Torgersen           NA            NA                  NA
View(penguins) # genera una tabla completa en otra ventana
?penguins # abre la página de ayuda

ggplot(data = penguins) # 1. Crea un lienzo en blanco listo para mostrar
# los datos de penguins
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g))
# 2. mapping es un argumento que recibe una función (ggplot es una 
# función de orden superior), esta función es aes que recibe el 
# argumento x y y y con eso se crea el eje de las x y el eje de las y
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + 
  geom_point() # 3. geom es el prefijo de las funciones de gráficos.
# geom_point se refiere a al gráfico de diagrama de dispersión de
# puntos 
# Entonces se observa una relación positiva lineal, es decir a medida
# que aumenta la longitud de la aleta, también aumenta la masa 
# corporal. Y esta relación es moderadamente fuerte pues no hay 
# demasiada dispersión alrededor de dicha línea. Es lineal porque
# los puntos se agrupan alrededor de una línea en lugar de una curva
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g,
                color = species) # Representamos la especie con colores
) +   
  geom_point()
# Representamos la especie de pinguinos con colores debido a que es 
# posible que la relación entre la aleta y la masa corporal varíe 
# según la especie. Notar que la especie es una variable categórica
# mientras que el largo de la aleta y la masa corporal son variables
# numéricas.

# Para agregar una línea curva suave se usará un nuevo objeto 
# geométrico geom_smooth  
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
# Especificaremos que queremos dibujar la línea con base en un modelo
# l lineal con method = "lm"

# Para que se dibuje una sola línea (y no tres) lo que se hace es
# quitar el argumento color de la función ggplot y lo pongo mapeando
# cada color en la función geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
# Con esto se mapean todas las especies de una sola vez.

# También se puede dar forma a cada valor para que no sea siempre y
# sólo un círculo con el parámetro shape en geom_point.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

# También se puede mejorar  o asignar etiquetas con la función labs
# (de labels) y sus parámetros
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") + 
  labs(title = "Masa Corporal y logitud de la aleta",
       subtitle = "Dimensiones de los pinguinos: Adelie, Chinstrap
       y Gentoo",
       x = "Longitud de la aleta (mm)",
       y = "Masa Corporal (g)",
       color = "Species", shape = "Species"
  ) +
  scale_color_colorblind() # cambia a colores para daltónicos

########################
###
### 1.2.5 Ejercicios
###
########################

# 1. ¿Cuántas filas tiene penguins? ¿Cuántas columnas?
# Mi parecer es que tiene 344 filas y 8 columnas


# 2. ¿Qué describe la bill_depth_mmvariable en el penguins marco de 
# datos? Consulta la ayuda para ?penguins averiguarlo.
?penguins # Parece que es el espesor de la aleta

# 3. Crea un diagrama de dispersión de bill_depth_mm vs.  
# bill_length_mm Es decir, crea un diagrama de dispersión con 
# bill_depth_mm en el eje y y bill_length_m men el eje x. Describe la 
# relación entre estas dos variables.
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                     color = species)
) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Longitud y Espesor de la alaeta",
       subtitle = "Dimensiones de los pinguinos: Adelie, Chinstrap y Gentoo",
       x = "Longitud de la aleta",
       y = "Espesor de la aleta",
       color = "Species", shape = "Species") +
  scale_color_colorblind()
# Este es un resultado que significa que separando especie por 
# especie la relación es positiva y lineal moderadamente fuerte pues
# no hay tanta dispersión
# Este es otro resultado:
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)
) + 
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(title = "Longitud y Espesor de la alaeta",
       subtitle = "Dimensiones de los pinguinos: Adelie, Chinstrap y Gentoo",
       x = "Longitud de la aleta",
       y = "Espesor de la aleta",
       color = "Species", shape = "Species") +
  scale_color_colorblind()
# Significa que tomando en cuenta las tres especies juntas la 
# relación es negativa y lineal , débil pues hay mucha dispersión

# 4. ¿Qué sucede si se crea un diagrama de dispersión de species vs.  
# bill_depth_mm? ¿Cuál sería una mejor opción de geometría?
ggplot(data = penguins,
       mapping = aes(x = species, y = bill_depth_mm)
) + 
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(title = "Especies y Espesor de la alaeta",
       subtitle = "Dimensiones de los pinguinos: Adelie, Chinstrap y Gentoo",
       x = "Especie",
       y = "Espesor de la aleta",
       color = "Species", shape = "Species") +
  scale_color_colorblind()
# Sería mejor conocer cuantos pinguinos hay en relación con el 
# espesor de la aleta, una especie de curva.

# 5. ¿Por qué aparece un error lo siguiente y cómo lo 
# solucionarías?
#ggplot(data = penguins) + 
#  geom_point()
# Pienso que el error es que no se les asigna valores a los ejes x
# e y, entonces no hay dónde dibujar los puntos

# 6. ¿Qué hace el na.rmargumento en geom_point()? ¿Cuál es su valor 
# predeterminado? Crea un diagrama de dispersión donde se use 
# correctamente este argumento establecido en TRUE.
?geom_point
# Se ve en la documentación que el valor predeterminado es FALSE
# Lo que hace na.rm es eliminar los valores faltantes predeterminados
# con una advertencia en el caso de FALSE, sin advertencia en el caso
# de TRUE
ggplot(
  data = penguins,
  aes(x = bill_depth_mm, y = bill_length_mm)
) +
  #geom_point(na.rm = FALSE) # con FALSE se ve el mensaje de que se
  # removieron dos filas con valores faltantes
  geom_point(na.rm = TRUE) # on TRUE no se ve el mensaje de que se 
  # removieron dos filas faltantes 

# 7. Añade el siguiente título al gráfico que hiciste en el ejercicio 
# anterior: “Los datos provienen del paquete palmerpenguins”. 
# Sugerencia: Echa un vistazo a la documentación de labs().
?labs()
ggplot(
  data = penguins,
  aes(x = bill_depth_mm, y = bill_length_mm)
) +
  geom_point(na.rm = TRUE) +
  labs(title = "Los datos provienen del paquete plamerpenguins")

# 8. Recrea la siguiente visualización. ¿ bill_depth_mmA qué estética 
# se debe asignar? ¿Y debería asignarse a nivel global o geom?
ggplot(
  data = penguins,
  aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth() +
  labs(title = "Los datos provienen del paquete palmerpenguins")
?geom_smooth()

# 9. Ejecuta este código mentalmente y predice el resultado. 
# Luego, ejecútalo en R y comprueba tus predicciones.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)
# Es un gráfico que en el eje de las x tiene los valores de 
# flipper_length_mm y en el eje de las y tiene body_mass_g y el
# color varía dependiendo de la isla dónde viven 
?geom_smooth
# se en geom_smooth por defecto es TRUE y muestra la banda de 
# confianza alrededor de la línea curva, Con FALSE ya no muestra 
# esa línea

# 10. ¿Se verán diferentes estos dos gráficos? ¿Por qué sí o por 
# qué no?
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )
# Realmente no sé si se ven iguales. Intuyo que sí porque en el primer
# gráfico se asignan como parámetros de ggplot data y mapping que
# deberían servir como base para llamar a las funciones geom_point y
# geom_smooth y en el segundo gráfico se asigna esos parámetros
# de manera explícita a las funciones geom. No estoy seguro de esto
# porque no parece haber algo parecido en scheme o lisp directamente
# parece un cambio de estado en las funciones geom, pero no estoy
# seguro.
# Al ejecutar veo que efectivamente produce los mismos gráficos.

# La siguente es una forma abreviada de generar un gráfico, sin llamar
# explícitamente a los parámetros data y mapping
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# Generar barras con variables categóricas
ggplot(penguins, aes(x = species)) +
  geom_bar()

# Genera barras ordenadas de frecuentes a infrequentes
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

# Genera un histograma con el peso e intervalos de 200 gramos
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200) # binwidth define el tamaño del 
                                 # intervalo

# Genera un histograma con el peso e intervalos de 20 gramos lo que 
# es demasiado pequeño y genera demasiadas barras
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20) # binwidth define el tamaño del 
# intervalo

# Genera un histograma con el peso e intervalos de 2000 gramos lo que 
# es demasiado grande y genera sólo 3 barras
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000) # binwidth define el tamaño del 
# intervalo

# Genera un gráfico de densidad, una versión suavizada de un 
# histograma. Sirve para comprender de manera rápida la fora de la
# distribución
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

########################
###
### 1.4.3 Ejercicios
###
########################

# 1. Crea un gráfico de barras species de penguins, donde asignas 
# species la y estética. ¿En qué se diferencia este gráfico?
ggplot(penguins, aes(y = species)) + 
  geom_bar()
# Cambian los ejes

# 2. ¿En qué se diferencian los siguientes dos gráficos? ¿Qué 
# estética, coloro fill, es más útil para cambiar el color de las 
# barras?
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")
# fill es la opción mejor para cambiar el color de las barras porque
# color solo cambia el color del borde. Y eso es un poco raro, nunca
# he visto eso

# 3. ¿Qué hace el bins argumento geom_histogram()?
?geom_histogram # bins es el número de intervalos, por defecto es 30
# Por ejemplo:
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram() 
# si no se define el tamaño del intervalo bindwidth, r ajustará el
# tamaño del intervalo para que haya 30 intervalos

# 4. Crea un histograma de la carat variable del diamonds conjunto 
# de datos disponible al cargar el paquete tidyverse. Experimenta 
# con diferentes anchos de bin. ¿Qué ancho de bin revela los 
# patrones más interesantes?
diamonds # produce:
# A tibble: 53,940 × 10
#    carat cut       color clarity depth table price     x     y     z
#    <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#  1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
#  2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
#  3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31

View(diamonds) # Abre la base de datos en una ventana nueva
ggplot(diamonds, aes(x = carat)) + # carat = kilate
  geom_histogram(bins = 80)
# En este caso specífico parece que muchos intervalos dan mejores
# visualizaciones porque los kilates son valores pequeños

# gráfico de un diagrama de caja. Los límites de la caja 
# representan el rango intercuartil (percentil 25-75). En el centro
# de la caja está la mediana (percentil 50). Estas tres líneas 
# permiten apreciar la dispersión de la distribución y si esta es 
# simétrica respecto a la mediana o si presenta un sesgo hacia un 
# lado.
# Hay unos puntos por fuera de la caja que representan valores 
# atípicos que se encuentra a más de 1.5 veces el rango intercuartil
# de cada borde del cuadro.
# También hay unas líneas o bigotes que llegan hasta el punto no atípico
# más alejado de la distribución.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
# Gráfico de la densidad
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75) # linewidth se usa para que las
# las líneas destaquen un poco más en contraste con el fondo 
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density() # sin linewidth las líneas se ven más pálidas
ggplot(penguins, aes(x = body_mass_g, color = species, 
                    fill = species)) +
  geom_density(alpha = 0.5) # alpha varía, de cero completamente 
# transparente a 1 completamente opaco
  
# Gráfico de barras apiladas para visualizar la relación entre dos
# variables categóricas. 
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
# El gráfico muestra la frecuencia de cada especie de pinguino en 
# cada isla. Sin saber los porcentajes por islas

# Este código genera barras comparativas por especies de pinguinos
# según las islas donde viven. También se usa para comparar 
# variables categóricas
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") # position = "fill" le dice a ggplot
# que genere barras con frecuencias relativas, lo que nos ayuda a
# ver los porcentajes de pinguinos  y relacionarlos por islas

# En cambio para visualizar la relación entre dos variables numéricas,
# se usa diagramas de dispersión (geom_point) y las curvas suaves
# (geom_smooth)
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() # produce un diagrama de dispersión

# Además se pueden incorporar más variables a un gráfico asignándoles
# elementos estéticos adicionales. Por ejemplo, en el siguiente 
# diagrama de dispersión, los colores de los puntos representan 
# especies y sus formas, islas.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
# Aunque no es tan buena opción tener demasiadas asignaciones estéticas
# porque el gráfico se vuelve confuso

# Otra opción, especialmente útil para variables categóricas, 
# es dividir el gráfico en facetas con facet_wrap(), subparcelas que 
# muestran cada una un subconjunto de los datos.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

########################
###
### 1.5.5 Ejercicios
###
########################

# 1. El mpgmarco de datos incluido en el paquete ggplot2 contiene 
# 234 observaciones recopiladas por la Agencia de Protección 
# Ambiental de EE. UU. en 38 modelos de automóviles. ¿Qué variables 
# mpg son categóricas? ¿Qué variables son numéricas? (Pista: 
# Escribe ?mpg para leer la documentación del conjunto de datos). 
# ¿Cómo puedes ver esta información al ejecutar mpg?
View(mpg)
?mpg
# categóricas: manufacturer, model, trans (transmisión), drv (tracción) 
# fl (tipo de gasolina), class (tipo de carro)
# numerícas: displ (desplazamiento del motor en litros), year, cyl
# (número de cilindros), cty (millas por galón en ciudad), 
# hwy (millas por galón en autopista), 

# 2. Crea un diagrama de dispersión de hwy vs displ usando el mpg
# marco de datos. A continuación, asigna una tercera variable 
# numérica a color, luego a size, luego a ambos color y size, 
# luego a shape. ¿Cómo se comportan estas variables de forma 
# diferente para las variables categóricas y numéricas?
ggplot(mpg, aes(x = hwy, y = displ)) +
  geom_point(aes(color = cty))

ggplot(mpg, aes(x = hwy, y = displ)) +
  geom_point(aes(size = cty))

ggplot(mpg, aes(x = hwy, y = displ)) +
  geom_point(aes(size = cty, color = cty ))

#ggplot(mpg, aes(x = hwy, y = displ)) +
#  geom_point(aes(shape = cty )) # produce: error

# Aparentemente color y size van cambiando de a poco en tono y tamaño
# cuando se usan variables numéricas , lo que es distinto a con su
# contrapartes categóricas. Parece que shape no se puede usar con
# variables numéricas porque produce un error.

# 3. En el diagrama de dispersión de hwy vs displ, ¿qué sucede si 
# asigna una tercera variable a linewidth?
ggplot(mpg, aes(x = hwy, y = displ, linewidth = cty)) +
  geom_point()
# Como no hay línea que se pueda modificar, el ancho es el mismo
               
# 4. ¿Qué pasa si asignas la misma variable a múltiples estéticas?
# Se usa esa misma variable para cambiar las estéticas y el gráfico
# resultante no resulta útil. Ejemplo: 
ggplot(mpg, aes(x = hwy, y = hwy, color = hwy)) + 
  geom_point()

# 5. Crea un diagrama de dispersión de bill_depth_mm vs 
# bill_length_mmy colorea los puntos con species. ¿Qué revela 
# añadir color por especie sobre la relación entre estas dos 
# variables? ¿Qué hay del facetado con species?
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(color = species)) #+ 
  #facet_wrap(~species)
# al añadir color se ve que el ancho de la aleta  del chinstrap es
# mayor que del gentoo pero el largo de la aleta es de igual tamaño.
# Se observa también que el ancho de la aleta de adelie y chinstrap 
# es igual pero el largo del chinstrap es mayor
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(color = species)) + 
  facet_wrap(~species)
# Añadir facetas no ayuda a comparar. 

# 6. ¿Por qué lo siguiente genera dos leyendas separadas? ¿Cómo se 
# solucionaría para combinarlas?
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
#Simple, le paso a la función labs el parámetro shape
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  #labs(color = "Species")
  labs(color = "Species", shape = "Species")

# 7. Crea los dos siguientes gráficos de barras apiladas. ¿Qué 
# pregunta puedes responder con el primero? ¿Qué pregunta puedes 
# responder con el segundo?
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
# Respondes la pregunta de qué porcentaje de pinguino de una especie
# hay en cada isla
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")
# Responde qué porcentaje de pinguinos de una especie vive en cada
# isla

# guardar un gráfico en el directorio actual con la función ggsave:
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")

########################
###
### 1.6.1 Ejercicios
###
########################

# 1. Ejecute las siguientes líneas de código. ¿Cuál de los dos 
# gráficos se guarda como mpg-plot.png? ¿Por qué?
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")
# El gráfico que se guarda es el último gráfico que se ejecutó

# 2. ¿Qué necesitas cambiar en el código anterior para guardar el 
# gráfico como PDF en lugar de PNG? ¿Cómo podrías averiguar qué 
# tipos de archivos de imagen son compatibles ggsave()?
ggsave("mpg-plot.pdf")
# habría que cambiar el subfijo .png por pdf
# Supongo que para saber qué archivos de imagen son compatible, 
# debería acudir a la documentación:
?ggsave 
# En device nos dice lo siguiente: Device to use. Can either be a 
# device function (e.g. png), or one of "eps", "ps", "tex" (pictex), 
# "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only). 
# If NULL (default), the device is guessed based on the filename 
# extension. 

# NOTA: 
# Lo siguiente está mal escrito:
#ggplot(data = mpg) 
#+ geom_point(mapping = aes(x = displ, y = hwy)) # produce: error
# el '+' debe ir al final, no al principio de una línea 
