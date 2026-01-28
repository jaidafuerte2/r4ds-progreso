source("r4ds-progreso/obesity-latin/R/load_data.R")

# Diagrama de dispersión que relaciona el imc con la edad tomando en 
# cuenta el tipo de obesidad
ggplot(
  data = obesity,
  mapping = aes(x = imc, y = Age, color = tipo_obesidad)
) +
  geom_point() +
  geom_smooth(method = "lm")

# Diagrama de dispersión que relaciona con la edad tomando en cuenta 
# el tipo de obesidad y con etiquetas
ggplot(
  data = obesity,
  mapping = aes(x = imc, y = Age)
) +
  geom_point(mapping = aes(color = tipo_obesidad, 
                           shape = tipo_obesidad)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Edad e imc",
    subtitle = "Edad según imc",
    x = "imc = Kg/m^2", y = "Edad en años",
    color = "tipo de obesidad", shape = "tipo de obesidad"
  )

# Grafico de barras para tipo de obesidad.
ggplot(obesity, aes(x = tipo_obesidad)) +
  geom_bar() +
  labs(
    title = "Gráfico de barras de",
    subtitle = "tipo de obesidad",
    x = "Tipo de obesidad",
    y = "cantidad"
  )

# Histogrma de edad
ggplot(obesity, aes(x = Age)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Histograma de",
    subtitle = "Edad",
    x = "edad", y = "cantidad"
  )

# Gráfico de densidad de edad
ggplot(obesity, aes(x = Age)) +
  geom_density() +
  labs(
    title = "Gráfico de densidad de",
    subtitle = "Edad",
    x = "Edad", y = "Densidad"
  )

# Diagrama de caja de edad y tipo de obesidad
ggplot(obesity, aes(x = tipo_obesidad, y = Age)) +
  geom_boxplot() +
  labs(
    title = "Diagrama de caja de",
    subtitle = "Tipo de obesidad y edad",
    x = "tipo de obesidad", y = "Edad"
  )

# Gráfico de densidad de edad y tipo de obesidad
ggplot(obesity, aes(x = Age, color = tipo_obesidad)) +
  geom_density(linewidth = 0.75)

# Gráfico de densidad de edad y tipo de obesidad con relleno por tipo
# de obesidad y opacidad
ggplot(obesity, aes(x = Age, color = tipo_obesidad,
                    fill = tipo_obesidad)) +
  geom_density(alpha = 0.5)

# Diagrama de dispersión que relaciona edad e índice de masa corporal
ggplot(obesity, aes(x = Age, y = imc)) +
  geom_point()

# Gráfico de barras que relaciona el tipo de obesidad con el género
ggplot(obesity, aes(x = tipo_obesidad, fill = Gender)) +
  geom_bar()

# Gráfico de barras que relaciona el tipo de obesidad con el género
# comparando la distribución entre tipo de obesidad
ggplot(obesity, aes(x = tipo_obesidad, fill = Gender)) +
  geom_bar(position = "fill")

# Gráfico de barras que relaciona el tipo de obesidad con el género
# comparando la distribución entre tipo de obesidad y etiquetando la 
# proporción
ggplot(obesity, aes(x = tipo_obesidad, fill = Gender)) +
  geom_bar(position = "fill") +
  labs(y = "Proporción")

# Diagrama de dispersión que relaciona la edad con el imc coloreado
#por tipo de obesidad y formateado por género
ggplot(obesity, aes(x = Age, y = imc)) +
  geom_point(aes(color = tipo_obesidad, shape = Gender))

# Diagrama de dispersión que relaciona la edad con el imc coloreado
# por tipo de obesidad y formateado por tipo de obesidad y facetado
# por género
ggplot(obesity, aes(x = Age, y = imc)) +
  geom_point(aes(color = tipo_obesidad, shape = tipo_obesidad)) +
  facet_wrap(~Gender)
