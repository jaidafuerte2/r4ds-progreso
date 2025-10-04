#####################################
###                               ###
###         Capítulo 6            ###
###      Flujo de Trabajo:        ### 
###     Scripts y proyectos       ###
###                               ###
#####################################

library(dplyr)
library(nycflights13)

# Te recomendamos que siempre inicies tu script con los paquetes que 
# necesitas. De esta forma, si compartes tu código con otros, podrán 
# ver fácilmente qué paquetes necesitan instalar. Sin embargo, ten en 
# cuenta que nunca debes incluirlo install.packages()en un script 
# que compartas. Es una falta de consideración entregar un script 
# que cambiará algo en su computadora si no se tiene cuidado.

not_canceled <- flights |>
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_canceled |> 
  group_by(year, month, day) |>
  summarize(mean = mean(dep_delay)) # produce:
# A tibble: 365 × 4
# Groups:   year, month [12]
#   year month   day  mean
#  <int> <int> <int> <dbl>
#1  2013     1     1 11.4 
#2  2013     1     2 13.7 
#3  2013     1     3 10.9 
#4  2013     1     4  8.97

# Por ejemplo, supongamos que tiene los siguientes archivos en una 
# carpeta de proyecto.
#alternative model.R
#code for exploratory analysis.r
#finalreport.qmd
#FinalReport.qmd
#fig 1.png
#Figure_02.png
#model_first_try.R
#run-first.r
#temp.txt
# Aquí hay una variedad de problemas: es difícil encontrar qué 
# archivo ejecutar primero, los nombres de los archivos contienen 
# espacios, hay dos archivos con el mismo nombre pero con diferente
# capitalización ( finalreport vs.  FinalReport ), y algunos nombres 
# no describen su contenido ( run-first y temp.txt).
# Aquí hay una mejor manera de nombrar y organizar el mismo conjunto 
# de archivos:
#01-load-data.R
#02-exploratory-analysis.R
#03-model-approach-1.R
#04-model-approach-2.R
#fig-01.png
#fig-02.png
#report-2022-03-20.qmd
#report-2022-04-02.qmd
#report-draft-notes.txt

getwd() # imprime el directorio actual de trabajo o current work 
# directory

