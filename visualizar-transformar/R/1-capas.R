library(tidyverse)

glimpse(mpg[1:4,]) # produce: 
#Rows: 4
#Columns: 11
#$ manufacturer <chr> "audi", "audi", "audi", "audi"
#$ model        <chr> "a4", "a4", "a4", "a4"
#$ displ        <dbl> 1.8, 1.8, 2.0, 2.0
#$ year         <int> 1999, 1999, 2008, 2008
#$ cyl          <int> 4, 4, 4, 4
#$ trans        <chr> "auto(l5)", "manual(m5)", "manual(m6)", "auto(av)"
#$ drv          <chr> "f", "f", "f", "f"
#$ cty          <int> 18, 21, 20, 21
#$ hwy          <int> 29, 29, 31, 30
#$ fl           <chr> "p", "p", "p", "p"
#$ class        <chr> "compact", "compact", "compact", "compact"

# Asignar explícitamente el color azul a los puntos del diagrama de
# dispersión y forma de rombo (que es la 9).
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue", shape = 9, size = 0.8)

# Diagrama de dispersión vs sínea suavizada que representan lo mismo
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_smooth()

# Diagrama de dispersión con línea savizada coloreada por drv
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point(size = 0.2) +
  geom_smooth()

# Línea suavizada coloreada por drv
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv))

# Diagrama de disersión coloreado por class y con una única línea 
# suavizada 
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

# Histograma revela que la distribución del kilometraje en carretera 
# es bimodal
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(binwidth = 2)
# Gráfico de densidad revela que la distribución del kilometraje en 
# carretera es bimodal
ggplot(mpg, aes(x = hwy)) +
  geom_density()
# Diagrama de caja revela dos posibles valores atípicos
ggplot(mpg, aes(x = hwy)) +
  geom_boxplot()

library(ggridges)
# Gráfico de crestas que sirve para visualizar las densidades de
# varaibles numéricas relacionadas con categóricas
ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)

# ejemplo de obtener ayuda de cualquier geom:
?geom_smooth

# Facetar dos variables
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free")

# Gráfico de barras de proporciones en lugar de recuentos
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

ggplot(diamonds, aes(x = cut)) + 
  geom_bar()
