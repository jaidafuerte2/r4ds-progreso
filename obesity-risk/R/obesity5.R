library(tidyverse)
source("r4ds-progreso/obesity-risk/R/load_data.R")
#View(obesity)
#getwd()

ggplot(obesity, aes(x = Gender)) +
  geom_bar()

ggplot(obesity, aes(x = family_history_with_overweight)) +
  geom_bar()

unique(obesity$family_history_with_overweight) # produce:
#[1] 1 0
obesity |> count(family_history_with_overweight) # produce:
#family_history_with_overweight     n
#                           <dbl> <int>
#1                              0  3744
#2                              1 17014

ggplot(obesity, aes(x = Gender, y = imc)) +
  geom_boxplot()

ggplot(penguins, aes(x = imc, color = family_history_with_overweight)) +
  geom_density(linewidth = 0.75)
