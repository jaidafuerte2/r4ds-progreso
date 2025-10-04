library(tidyverse)

getwd() # produce: [1] "/cloud/project"

ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_hex()
ggsave("whole-game/data/diamonds.png")

write_csv(diamonds, "whole-game/data/diamonds.csv")
