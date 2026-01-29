source("r4ds-progreso/obesity-latin/R/load_data.R")

# Visualizar barras de tipo de obesidad en orden descendente
ggplot(obesity, aes(x = fct_infreq(tipo_obesidad))) +
  geom_bar()

# Diagrama de densidad que relaciona la edad con el tipode obesidad
ggplot(obesity, aes(x = Age, color = tipo_obesidad)) +
  geom_density(linewidth = 0.75)

# Diagrama de densidad que relaciona la edad con el tipo de obesidad
# y una transparencia de 0.5
ggplot(obesity, aes(x = Age, color = tipo_obesidad, 
                    fill = tipo_obesidad)) +
  geom_density(alpha = 0.5)

# Los nombres de las variables y otros se separan con guiones bajos
# como en python, no con guiones medios como en lisp

# Cambiar el nombre de las variables
obesity_renamed <- obesity |>
  rename(
    peso = Weight,
    altura = Height
  )
obesity_renamed[1:4,] # produce:
# A tibble: 4 × 19
#  Gender   Age altura  peso family_history_with_…¹ FAVC   FCVC   NCP CAEC 
#  <chr>  <dbl>  <dbl> <dbl> <chr>                  <chr> <dbl> <dbl> <chr>
#1 Female    21   1.62    64 yes                    no        2     3 Some…
#2 Female    21   1.52    56 yes                    no        3     3 Some…
#3 Male      23   1.8     77 yes                    no        2     3 Some…
#4 Male      27   1.8     87 no                     no        3     3 Some…

