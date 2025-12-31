source("r4ds-progreso/obesity-risk/load_data.R")
library(tidyverse)
View(obesity)


ggplot(
  data = obesity,
  mapping = aes(x = imc, y = Age, color = tipo_obesidad)
) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(
  data = obesity,
  mapping = aes(x = imc, y = Age)
) +
  geom_point(mapping = aes(color = tipo_obesidad, shape = tipo_obesidad)) + 
  geom_smooth(method = "lm") +
  labs (
    title = "Edad e IMC",
    subtitle = "Edad según IMC",
    x = "IMC (Kg/m^2)", y = "Edad en años",
    color = "Tipo de obesidad", shape = "Tipo de obesidad" 
  )

# Gráfico de barras para tipo de obesidad
ggplot(obesity, aes(x = fct_infreq(tipo_obesidad))) +
  geom_bar()

# Histograma de edad
ggplot(obesity, aes(x = Age)) +
  geom_histogram(binwidth = 1)

# Gráfico de densidad de edad
ggplot(obesity, aes(x = Age)) +
  geom_density()

# Diagrama de caja de Edad y tipode obesidad
ggplot(obesity, aes(x = tipo_obesidad, y = Age)) +
  geom_boxplot()

# Gráfico de densidad de Edad y tipo de obesidad
ggplot(obesity, aes(x = Age, color = tipo_obesidad)) +
  geom_density(linewidth = 0.75)

# Gráfico de densidad de Edad y tipo de obesidad con relleno y opacidad
ggplot(obesity, aes(x = Age, color = tipo_obesidad, 
                    fill = tipo_obesidad)) +
  geom_density(alpha = 0.5)

# Diagrama de dispersión que relaciona edad e imc
ggplot(obesity, aes(x = Age, y = imc)) +
  geom_point()

# Relacionar género, Edad con imc y tipo de obesidad con un diagrama de 
# dispersión
ggplot(obesity, aes(x = imc, y = Age)) +
  geom_point(aes(color = tipo_obesidad, shape = Gender))

# Facetar 3 las vriables imc, Age y tipo de obesidad
ggplot(obesity, aes(x = imc, y = Age)) +
  geom_point(aes(color = tipo_obesidad, shape = tipo_obesidad)) +
  facet_wrap(~Gender)
